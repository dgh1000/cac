
module Translation.Translation where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Common.CommonUtil as CU
import Data.Ratio
import Debug.Trace
import Control.Arrow
import Control.Monad.State
import Control.Monad.Writer
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import System.Random
import Data.Array.IArray
import Data.Monoid
import Data.Map(Map)
import Data.Maybe
import Midi.MidiData
import Common.CommonExport
import Common.CommonUtil           (locDiff,simpleShowLoc)
import Translation.TranslationData
import Translation.Dynamics        (makeCompleteLoudnessFunc)
import Translation.Warp            (applyWarps, applyPauses, applyAbsWarps)
import Translation.TimeMap         (lookupTime,toAbsolute,computeBaseTimeMap
                                   ,alterTimeMapByPattern)
import Translation.Mod             (writeMod)
import Translation.Splice          (cut)
import Translation.AlterTOff
import Translation.Trill           (trillTimes)
import Util.Exception
import Util.Map                    ( splitInclude,lookupMaxLower
                                   , lookupMinUpperWithKey,lMapToList
                                   , listToLMap)
import Util.Math                   (scale)
import Util.Exception
import Score.ScoreData


{-

more efficient storage of events before starting playback

  maybe keep mod in an array before playback so don't have to sort it later

  where are events generated?
  
    Translation.hs 

      SingleEvents for pedal

      NoteEvents for chords and trill/trem

    Mod.hs

      SingleEvents for mod wheel


how should I communicate the idea of whether a note should be extended or
truncated?


  nomenclature for notes of same pitch. Given two notes X Y of same pitch, Y
  closely following X

    T_offEarly is the amount of time X's midi off event must occur before Y's
    midi on event so they won't stomp on each other. Generally this is 1 ms.

    T_repSpace is T_Y_on - T_X_off chosen large enough for Y attack to be
    audible

      x is sustained sound

        y is sustained sound: need significant T_repSpace, at least 50 ms

        y has sharp attack (pizz, short, piano note): would only need
        T_repSpace if x is as loud as attack of y: would almost never occur

      x is pizz, short, or piano note: these are sounds that die away
      quickly. 

        y is sustained sound: would need separation if x hasn't decayed much
        and y is very quiet. rare case.

        y is pizz, short, or piano note: I can't image a case that would need
        separation

so basic truncation algorithm is, for Y following X, same pitch: (1) if X and
Y are both sustained type sounds, truncate X so that they are separated by
T_repSpace. (2) otherwise truncate X so they are separated by 1 ms




do we ever need to apply both note extension and note truncation in the same
instrument?

  when is note extension useful

    note extension applys in piano at least

    can note extension apply to Garriton or Quantum leap?

      sounds wrong with sustained sounds, especially with QL reverb tails
      method

      the legato script needs some of kind of note extension, but I don't
      think I've ever gotten legato script to sound good

  so note extension only useful in piano, for now. and no truncation is useful
  for piano

  conclusion: note extension and truncation never apply to same instrument





  all notes followed by same note should be truncated at least 1 ms so that
  midi off event doesn't stomp on following on event

  what notes should be truncated more than 0.001

    when immediately following note is same pitch


okay we need to add some timing variation

  main problem to solve

    randomness. need to have random seed threaded through state monad. oh
    already have that in TrState

    time map alteration.

      based on where notes occur. need to have access to that while altering
      time map

      decide when to alter time map relative to warps, pauses, ramps

        lets say: tempo (ramps and set tempo), then warps/pauses, then
        variation

          okay in that case warps and pauses are not altered by time map
          changes

        let's say variation before warps/pauses

          well in that case interesting, the random variation will affect
          warps and pauses, which might be of benfit. let's see if we can
          configure it either way



-}





scoreToMidi :: PlaybackConfig -> Score -> StdGen -> 
               ([DebugItem],[EventsRecord],TrState)
scoreToMidi plConf score gen = (outputItems,records,finalSt)
  where
    tempoRatio        = pcTempoChange plConf
    transposition     = pcTranspose plConf
    state             = TrState plConf score M.empty gen []
    (records,finalSt) = runState (scoreToMidi' tempoRatio transposition) state
    outputItems       = []


scoreToMidi' :: Double -> Int -> Tr [EventsRecord]
scoreToMidi' tempoRatio transp = do

  -- Determine patterns in time and dynamics here? do we have everything we
  -- need? we have PlaybackConfig, yes. we have score. so we compute some
  -- structure that represents all dynamics and time alterations. 
  -- 
  -- time maps: is

  -- Compute measure range to convert.
  (mBeg,mEnd) <- computeMsrRange
  score       <- gets tsScore

  -- Now compute the timemap and loudness function for each staff (the data
  -- in PerStaff), which needs the base time map.
  -- pat <- pcPattern `liftM` gets tsPlaybackConfig
  baseTimeMap <- computeBaseTimeMap tempoRatio
  baseTimeMap <- computeBaseTimeMap tempoRatio
  perSt <- forM (M.keys . scStaves $ score) (computePerStaff baseTimeMap)
  modify (\s -> s {tsPerStaff = M.fromList perSt})

  -- For each staff, compute one NoteEventsRecord and one FixedEventsRecord
  -- write staves and accumulate ranges
  --
  -- What if we mute here? Just don't convert a staff? Does writeStaff have
  -- access to the config file? This gets staves from score. So we have to
  -- look up if one is in the config.
  staffResults <- mapM (writeStaff mBeg mEnd) (M.elems . scStaves $ score)
  let (ranges,erList) = unzip staffResults
      (lMin,lMax) = case catMaybes ranges of
        [] -> throwMine $ printf "No notes between msrs %d and %d" mBeg mEnd
        rs -> let (mins,maxs) = unzip rs in (minimum mins,maximum maxs)

  

  -- compute pedal events
  pedERList <- writeSusPed lMin lMax

  -- compute mod events as a series of time-ordered lists
  modERList <- mapM (writeMod lMin lMax) (M.keys $ scStaves score)

  -- splicing
  let allRecords :: [EventsRecord]
      allRecords = (map (doTranspose transp) . concat $ erList) ++ pedERList ++ 
                   modERList
  spl <- pcSplice `liftM` gets tsPlaybackConfig
  splicedRecords <- case spl of
    Nothing -> return allRecords
    Just x -> do
      locPairs <- searchSplicePoints mBeg mEnd (fromJust spl)
      spliceAll allRecords locPairs

  -- alter tOff
  mapM alterTOff_record splicedRecords


doTranspose :: Int -> EventsRecord -> EventsRecord
doTranspose t (NoteEventsRecord n evts) = 
  NoteEventsRecord n (map (transposeMidi t) evts)
doTranspose _ e = e


-- how do we use pattern information in constructing loudness funcs and
-- timemap alterations?
--
-- time patterns can be put into baseTimeMap because they affect every staff
-- the same
computePerStaff :: RelTimeMap -> String -> Tr (String,PerStaff)
computePerStaff baseTimeMap staffName = do
  score  <- gets tsScore
  staff  <- getStaffTr staffName
  tm     <- ( toAbsolute `liftM` 
              staffRelTimeAlter staffName baseTimeMap
            ) >>= staffAbsTimeAlter staffName
  instr  <- getInstrTr   staffName
  pat    <- getPatternTr
  let numMeasures = length $ scTimeSigs score
      (loudFuncs,lfDebug) = makeCompleteLoudnessFunc numMeasures instr pat 
                            staff tm
  return ( staffName , PerStaff tm loudFuncs lfDebug)


staffRelTimeAlter :: String -> RelTimeMap -> Tr RelTimeMap
staffRelTimeAlter staffName tm = do
  globMarks <- (lMapToList . scMarks) `liftM` gets tsScore
  let globalWarps = filter (isGlobalWarp . snd) globMarks
      absWarps = filter (isAbsWarp . snd) globMarks
      pauses = filter (isPause . snd) globMarks
  staffMarks <- (lMapToList . stMarks) `liftM` getStaffTr staffName
  let staffWarps = filter (isStaffWarp . snd) staffMarks
  (applyWarps (globalWarps++staffWarps) tm >>= 
   applyAbsWarps absWarps >>=
   applyPauses pauses)


staffAbsTimeAlter :: String -> AbsTimeMap -> Tr AbsTimeMap
staffAbsTimeAlter staffName tm = return tm
  -- do
  -- flag <- isSineWaveStaff staffName 
  -- (if flag then sineWaveTime else return) tm


----------------------------------------------------------------------
----------------------------------------------------------------------
--                     muting staves

{-

doMuting :: PlaybackConfig -> [OutputItem] -> [OutputItem]
doMuting plConf items =
    filter (not . isOutputItemMute) items
  where
    ms = destMutes . M.elems . pcStaffConfigs $ plConf
    isOutputItemMute i = case lookupItemDest i of
      Nothing -> False
      Just x  -> decideIfMute ms x


decideIfMute :: Map (Int,Int) MuteState -> (Bool,(Int,Int)) -> Bool
decideIfMute m (noteFlag,dest) = 
  case (fromJust . M.lookup dest $ m, noteFlag) of
    (MuteAll        ,    _) -> True
    (MuteNone       ,    _) -> False
    (MuteNotes      , flag) -> flag
    (MuteControllers, flag) -> not flag


lookupItemDest :: OutputItem -> Maybe (Bool,(Int,Int))
lookupItemDest i = case i of
  OutputNote {}    -> Nothing
  OutputSusPed {}  -> Nothing
  OutputMidiEvent (SingleEvent _ _ raw) ->
     Just (False,(rmeStream raw,rmeChan raw))
  OutputMidiEvent (NoteEvent _ _ on _ _ _ _ _) -> 
     Just (True, (rmeStream on,rmeChan on))
    

destMutes :: [StaffConfig] -> Map (Int,Int) MuteState
destMutes = M.unions . map doOne
  where
    doOne StaffConfig {stcMute=mute, stcMidiDests=dests} = 
      M.fromList . map (\d -> (d,mute)) . M.elems $ dests
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--         Finding the range of measures to play... looking for
--         three empty consecutive measures and then splice points
--         within those measures






computeMsrRange :: Tr (Int,Int)
computeMsrRange = do
  score <- gets tsScore
  (confBeg,maybeConfEnd) <- pcMsrRange `liftM` gets tsPlaybackConfig
  let endMsr = case maybeConfEnd of
        Just x  -> x
        Nothing -> findEndMsr score confBeg
  return (confBeg,endMsr)


findEndMsr :: Score -> Int -> Int
findEndMsr score begMsr
  | begMsr > (snd $ bounds a) = throwMine ("Given beginning measure is past "++
                                "the end of the score")
  | otherwise = 
      let x = searchNBlanks begMsr 3 . drop (begMsr-1) . 
              (++ [False,False,False,False]) . elems $ a
      in ("End measure:" ++ show x) `trace` x
  where
    a = scUsedMsrs score


searchNBlanks :: Int -> Int -> [Bool] -> Int
searchNBlanks currMsr n bs
  | L.isPrefixOf (replicate n False) bs = currMsr - 1
  | otherwise = searchNBlanks (currMsr+1) n (drop 1 bs)



{-

new algorithm

  for each staff

    find cut points as Integer milliseconds

    call cut on notes, Map Integer [MidiEvent]

    call cutSingles on mod events, [MidiEvent]

don't need partitionMidi becauase already partitioned by staff

where in the Translation process do we compute the "end mod midi events", the
ones that in Quantum Leap will set up a legato or portamento connection? it
has to be in the instrument. where is the instrument called? in Translation we
have a chord or a trill/tremolo.  

-}

{-
spliceMidi :: (Loc,Loc) -> Tr ()
spliceMidi locs = do
  staffNames <- (M.keys . scStaves) `liftM` gets tsScore
  mapM_ (spliceMidiStaff locs) staffNames
-}


spliceAll :: [EventsRecord] -> [(Loc,Loc)] -> Tr [EventsRecord]
spliceAll ers locPairs = do
  let f :: EventsRecord -> Tr EventsRecord
      f er = foldM spliceEvents er (reverse $ L.sort locPairs)
  mapM f ers


spliceEvents :: EventsRecord -> (Loc,Loc) -> Tr EventsRecord
spliceEvents r@FixedEventsRecord{} _ = return r
spliceEvents (SusPedEventsRecord staffName es) locs =
  SusPedEventsRecord staffName `liftM` spliceStaff False locs staffName es
spliceEvents (NoteEventsRecord staffName es) locs =
  NoteEventsRecord   staffName `liftM` spliceStaff True locs staffName es
spliceEvents (ModEventsRecord staffName es) locs =
  ModEventsRecord    staffName `liftM` spliceStaff False locs staffName es


spliceStaff :: Bool -> (Loc,Loc) -> String -> [MidiEvent] -> Tr [MidiEvent]
spliceStaff needsHealing (loc1,loc2) staffName evts = do
  tm <- getPerStaffTr tpsTimeMap staffName
  let t1 = round $ 1000 * lookupTime loc1 tm
      t2 = round $ 1000 * lookupTime loc2 tm
  return $ cut needsHealing t1 t2 evts

{-
  modify (\s@TrState {  tsMidiEvts = me
                     ,  tsModEvts  = mod} -> 
           s { tsMidiEvts = M.update (Just . cut t1 t2) staffName me
             , tsModEvts  = M.update (Just . cutSingles t1 t2) staffName mod })
-}



filterSplicePoints :: Map Loc [Mark] -> [(Loc,Char)]
filterSplicePoints = mapMaybe g . lMapToList
  where
    g (loc,SpliceMark c) = Just (loc,c)
    g _                  = Nothing


searchSplicePoints :: Int -> Int -> Char -> Tr [(Loc,Loc)]
searchSplicePoints m1 m2 goalChar = do
  splicePts <- (filterSplicePoints . scMarks) <$> gets tsScore
  let lMin     = Loc m1 1
      lMax     = Loc (m2+1) 1
  case L.find (\(l,c) -> lMin<=l && l<lMax && c=='a') splicePts of
    Nothing    -> throwMine "no 'a' found"
    Just (x,_) -> do
      searchSplicePoints2 lMin lMax goalChar x splicePts


searchSplicePoints2 :: Loc -> Loc -> Char -> Loc -> [(Loc,Char)] -> 
                       Tr [(Loc,Loc)]
searchSplicePoints2 lMin lMax goalChar lA markers = do
  let lGoal    = case L.find (\(l,c) -> l >= lA && c==goalChar) markers of
                   Nothing    -> throwMine $ printf ("Unable to find goal " ++
                                 "marker '%s'") [goalChar]
                   Just (x,_) -> x
      lGoalEnd = case L.find (\(l,_) -> l > lGoal) markers of
                   Nothing    -> lMax
                   Just (x,_) -> x
  return $ filter (\(x,y) -> x/=y) [(lA,lGoal),(lGoalEnd,lMax)]


{-
maybeMarker :: [TextCmd] -> Maybe Char
maybeMarker = getFirst . mconcat . map (First . maybeOneMarker)


maybeOneMarker :: TextCmd -> Maybe Char
maybeOneMarker (MarkerCmd c) = Just c
maybeOneMarker _             = Nothing
-}

maybeSpliceMark :: Mark -> Maybe Char
maybeSpliceMark (SpliceMark c) = Just c
maybeSpliceMark _              = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--              writing staves, chords, notes


-- writeStaff
--
-- Needs to write MIDI events corresponding to notes in this staff that begin
-- within the range of mBeg to mEnd. Also needs to compute the minimum Loc
-- of any note, and the maximum true end loc of any note.
--
-- First must create some data structures that are useful for 
-- Output 
--
--   Maybe (Loc,Loc) :: is Nothing if no notes were found at all, and Just
--                      (<min loc>, <max end loc>) if notes were computed
--   [MidiEvent] :: events generated by notes
--   [MidiEvent] :: track start events
--
writeStaff mBeg mEnd staff = do
  ignoreStaffNames <- pcStaffIgnore `liftM` gets tsPlaybackConfig
  if S.member (stName staff) ignoreStaffNames
    then return $ (Nothing,[])
    else writeStaff_2 mBeg mEnd staff


writeStaff_2 :: Int-> Int -> Staff -> Tr (Maybe (Loc,Loc),[EventsRecord])
writeStaff_2 mBeg mEnd staff = do
  (bounds,noteEvts) <- case getChordKeysMsrRange mBeg mEnd staff of
    Just (minLoc,maxLoc,keys) -> do 
      evts <- concat `liftM` forM keys writeChord
      return $ (Just (minLoc,maxLoc),evts)
    Nothing -> return (Nothing,[])
  fixedEvts <- writeFixedControllers (stName staff)
  let name = stName staff
  return (bounds, [ NoteEventsRecord  name (L.sort noteEvts )
                  , FixedEventsRecord name (L.sort fixedEvts) ])


-- getChordKeysMsrRange
--
-- Construct a chord key for every chord that has a begin location between
-- measure mBeg (inclusive) and mEnd (exclusive). Evalutates to Just (<min
-- beg>, <max true end>, [<key>]) if there are any keys in the given range,
-- otherwise evaluates to Nothing
--
getChordKeysMsrRange :: Int -> Int -> Staff -> Maybe (Loc,Loc,[ChordKey])
getChordKeysMsrRange mBeg mEnd staff = 
  case M.minViewWithKey chordsInRange of
    Nothing        -> Nothing
    Just ((k,_),_) -> Just (k, maximum . map getMaxTrueEnd $ keysOut, keysOut)
  where
    keysOut = concatMap doLoc . M.toList $ chordsInRange
    locBeg = Loc mBeg     1
    locEnd = Loc (mEnd+1) 1
    chordsInRange = 
      fst . M.split locEnd . snd . splitInclude locBeg $ (stChords staff)
    doLoc :: (Loc,Map Int Chord) -> [ChordKey]
    doLoc (loc,chords) = map doVoice . M.toList $ chords
      where
        doVoice :: (Int,Chord) -> ChordKey
        doVoice (vn,ch) = ChordKey (stName staff) loc vn ch


writeChord :: ChordKey -> Tr [MidiEvent]
writeChord chordKey = do
  let dtNotes = getDoubTremNotes chordKey
  trillStatus <- maybeTrill chordKey
  let case_ | not (null dtNotes) = TremoloCase
            | otherwise = case trillStatus of
                Nothing         -> SingleCase (getChordLoc chordKey)
                Just alter      -> TrillCase alter
  case case_ of
    SingleCase _ -> doSingleChord      chordKey
    x            -> doTrillTremChord x chordKey

  
computeDoubTremPitches :: ChordKey -> ([Int],[Int])
computeDoubTremPitches c = ( map getMidiPitch (getChordNotes c)
                           , map getMidiPitch (getDoubTremNotes c) )


computeTrillPitches :: Int -> ChordKey -> ([Int],[Int])
computeTrillPitches alter chordKey = case getChordNotes chordKey of
  [noteKey] -> ( [getMidiPitch noteKey] 
               , [computeTrillPitch alter (getPitch noteKey)] )
  xs -> throwMine $ printf ("Can't apply trill to chord with multiple notes "++
        "at %s") (simpleShowLoc . getChordLoc $ chordKey)


computeTrillPitch :: Int -> Pitch -> Int
computeTrillPitch upperAlter (Pitch _ step _ octave) = newMidiPitch
  where
    newStep = (step+1) `mod` 7
    newOctave = if step == 6 then octave+1 else octave
    newMidiPitch = CU.stepAlterOctToMidi newStep upperAlter newOctave


-- Check if a chord specifies a trill. If so return (Just <alter>) where
-- alter is 0 for a trill with natural sign, -1 for trill with flat sign,
-- and 1 for trill with sharp sign. This refers to the alteration on the 
-- upper named trill pitch.
maybeTrill :: ChordKey -> Tr (Maybe Int)
maybeTrill (ChordKey staffName loc voiceNum _)= do
  marks <- stMarks `liftM` getStaffTr staffName
  let maybeTrill :: Mark -> Maybe Int
      maybeTrill (SymbolMark s vn)
        | vn /= voiceNum = Nothing
        | otherwise = case s of
                        "trill-natural" -> Just 0
                        "trill-flat"    -> Just (-1)
                        "trill-sharp"   -> Just 1
                        "Trill"         -> Just 0
                        _               -> Nothing
      maybeTrill _ = Nothing
  return (M.lookup loc marks >>= getFirst . mconcat . map (First . maybeTrill))


-- The instrument is responsible for coming up with destination. So we change
-- that to array for multiple destinations.
doSingleChord :: ChordKey -> Tr [MidiEvent]
doSingleChord chordKey = forM (getChordNotes chordKey) doSingleNote
  where
    doSingleNote noteKey = do
      let stNa          =  ckStaffName chordKey
          pit           =  getMidiPitch noteKey
      instr             <- getInstrTr stNa
      -- tBeg and tEnd are used to create the NoteEvent
      -- so these could come from tModel
      -- tModel is given to DebugNote, no need to change that
      tModel            <- (iTimeFn instr) noteKey
      let tBeg          = tmActTBeg tModel
          tEnd          = tmActTEnd tModel
      Just lfn          <- M.lookup (getVoiceNum chordKey) `liftM`
                           getLoudnessFuncsTr stNa
      let loudness      =  lfn (Left $ getChordLoc chordKey)
          loc           =  getChordLoc chordKey
      vel               <- (iVelFn  instr) (SingleCase loc) loudness chordKey
      let (stream,chan,susFlag) =  (iDestFn instr) (SingleCase loc) chordKey
      let note x        =  RawMidiEvent stream chan x pit vel
          noteOn        =  note 0x90
          noteOff       =  note 0x80
      tellDebugOutput $ DebugNote chordKey (SingleCase loc) tModel
                        (SinglePitchRecord pit) (Just loudness)
      -- As coded now, we don't put any note modifiers. What are modifier
      -- examples? Probably keyswitches and patch selections in my chamber
      -- music program. Legato flag would be computed where? In conversion to
      -- raw?
      return $ NoteEvent (round $ 1000*tBeg) (round $ 1000*tEnd) 
               noteOn noteOff [] False [] stNa susFlag 
               (isJust . tmStacDur $ tModel)


data TrillData = TrillData Double Double TrillStep Bool
            -- ^ tBeg, tEnd, Upper or Lower, <true means use loc in call to
            --   loudness func>
            -- For tremolo, Lower means first chord and Upper means second 
            -- chord.
               deriving(Show)


--
-- case_ :: (TrillCase alter) or (TremoloCase)
doTrillTremChord :: TranslationCase -> ChordKey -> Tr [MidiEvent]
doTrillTremChord case_ chordKey@(ChordKey staffName loc vn _) = do
  -- in trill:   pitches1 contains lower pitch, pitches2 higher pitch
  -- in tremolo: pitches1 contains first chord, pitches2 second chord
  let (pitches1,pitches2) = case case_ of
        TrillCase alter -> computeTrillPitches alter chordKey
        TremoloCase     -> computeDoubTremPitches    chordKey
  timeMap    <- getTimeMapTr staffName
  let tBeg   =  lookupTime loc timeMap
      n1:_   =  getChordNotes chordKey
      tEnd   =  lookupTime (getTrueEnd n1) timeMap
  instr      <- getInstrTr staffName
  let shape = snd $ fromJust $ M.lookupLE loc $
        (case case_ of TrillCase _ -> iTrillShape; 
                       TremoloCase -> iTremShape) instr
      times = trillTimes shape tBeg tEnd
      steps =  zipWith (\(t1,t2,s) flag -> TrillData t1 t2 s flag) times
                       (True:repeat False)
      doStep :: TrillData -> Tr [MidiEvent]
      doStep (TrillData t1 t2 s flag) = do
        let thePitches = if s == Lower then pitches1 else pitches2
            doPitch p = doTrillTremOneNote case_ chordKey p t1 t2 flag
        mapM doPitch thePitches
  tellDebugOutput $ DebugNote chordKey case_
                    (TrillTremModel tBeg tEnd)
                    (TrillTremPitchRecord pitches1 pitches2) Nothing
  concat <$> mapM doStep steps
 

{-
      doPitch :: [((Double,Double),Bool)] -> Int -> Tr [MidiEvent]
      doPitch timesFlags p = 
            forM timesFlags (doTrillTremOneNote case_ chordKey p)
  ps1<- concat `liftM` forM pitches1 (doPitch (zip times1 (True:repeat False)))
  ps2<- concat `liftM` forM pitches2 (doPitch (zip times2 (     repeat False)))
  Just lfn   <- liftM (M.lookup vn) $ getLoudnessFuncsTr staffName
  let begLoudness = lfn (Left loc)
  tellDebugOutput $ DebugNote chordKey case_ 
                    (TrillTremModel tBeg tEnd)
                    (TrillTremPitchRecord pitches1 pitches2) begLoudness
  return $ ps1 ++ ps2
-}


computeTrillTremTimes :: Double -> Double -> Double -> 
                         ([(Double,Double)], [(Double,Double)])
computeTrillTremTimes tBeg tEnd rate = 
    (everyOther times, everyOther (tail times))
  where
    dur            = tEnd - tBeg
    approxNumNotes = rate * dur
    numNotes       = max 2 (2 * round (rate*dur/2))
    noteDur        = dur / fromIntegral numNotes
    times = map (\i -> tBeg+noteDur*fromIntegral i) [0..numNotes]
    everyOther (x:y:remain) = (x,y) : everyOther remain
    everyOther _            = []


doTrillTremOneNote :: TranslationCase -> ChordKey -> Int -> Double -> Double ->
                      Bool -> Tr MidiEvent
doTrillTremOneNote case_ chordKey pitch tBeg tEnd first = do
  let staffName =  getStaffName         chordKey
  instr         <- getInstrTr           staffName
  Just lfn      <- M.lookup (getVoiceNum chordKey) `liftM` 
                     getLoudnessFuncsTr staffName
  let chordLoc  =  getChordLoc chordKey
      loudness  = if first then lfn (Left chordLoc) else lfn (Right tBeg)
  vel           <- (iVelFn  instr) case_ loudness chordKey
  let (stream,chan,susFlag) = (iDestFn instr) case_ chordKey
      note x    = RawMidiEvent stream chan x pitch vel
      noteOn    = note 0x90
      noteOff   = note 0x80
  return  $ NoteEvent (round $ 1000*tBeg) (round $ 1000*tEnd) 
                      noteOn noteOff [] False [] staffName susFlag False


----------------------------------------------------------------------
----------------------------------------------------------------------
--              writing non-notes

{-
writeNonNotes beg end = do
  st <- gets (scStaves . tsScore)
  mapM (writeMod beg end) (M.keys st)
  mapM writeFixedControllers (M.keys st)
  writeSusPed beg end
-}  

writeFixedControllers :: String -> Tr [MidiEvent]
writeFixedControllers staffName = do
  i <- getInstrTr staffName
  let writeControl :: ((Int,Int),(Int,Int)) -> MidiEvent
      writeControl ((stream,chan),(contr,value)) =
        SingleEvent 0 True (RawMidiEvent stream chan 0xb0 contr value) 
                    staffName
  return $ map writeControl (iFixedControls i)


writeSusPed :: Loc -> Loc -> Tr [EventsRecord]
writeSusPed beg end = do
  mSusPed <- gets (pcSusPed . tsPlaybackConfig)
  case mSusPed of
    Nothing -> return []
    Just c@(SusPedConfig _ _ src app) -> do
      pedEvts <- (clipPedEvts beg end . stPedalEvts) `liftM` getStaffTr src
      forM app (\n -> do
        i <- getInstrTr n
        case iSusPedDest i of
          Nothing -> throwMine $ printf ("Staff %s is specified as sus ped"++
                     " application staff, but its instrument provides no "++
                     "sus ped destination")
          Just dest -> writeSusPedEvts pedEvts c n dest)


-- clipPedEvts
--
-- Given all pedal events in staff, isolate just those between 'beg' and 'end'
-- exclusive. Call this set 'middle'
--
-- Then possibly add an event at 'beg' as follows:
--
--  1. find first event at or before 'beg':
--       a. if it's a start or a Change,   add a start
--       c. if it's a stop,                add Nothing
--  2. if none exists,                     add nothing
--
-- Then possibly add an event at the first beat of the measure following 'end'
-- as follows: call this loc end2
--
--  1. look at pedal event in the entire staff most immediately at or before 
--     'end'
-- 
--       a. if it's a down or change,      add Up at end2
--       b. if it's an up,                 add nothing
--       c. if none exists                 add Nothing
clipPedEvts :: Loc -> Loc -> Map Loc PedalEvt -> Map Loc PedalEvt
clipPedEvts beg end evts = M.unions [middle,newBeg,newEnd]
  where
    middle = fst . M.split end . snd . M.split beg $ evts
    newBeg = case M.lookupLE beg evts of
      Nothing                     -> M.empty
      Just (_,e) | e == PedalStop -> M.empty
                 | otherwise      -> M.singleton beg PedalStart
    newEnd = case M.lookupLE end evts of
      Nothing                     -> M.empty
      Just (_,e) | e == PedalStop -> M.empty
                 | otherwise      -> M.singleton (beatOneNextMsr end) PedalStop


beatOneNextMsr (Loc m _) = Loc (m+1) 1


writeSusPedEvts :: Map Loc PedalEvt -> SusPedConfig -> String -> (Int,Int) -> 
                   Tr EventsRecord
writeSusPedEvts evts (SusPedConfig liftDelta liftDur _ _) staffName 
  (stream,chan) = do
    tm <- getTimeMapTr staffName
    let toCode PedalStart = 127
        toCode PedalStop  = 0
        toMidi t evt = SingleEvent (round $ 1000*t) False
           (RawMidiEvent stream chan 0xb0 0x40 (toCode evt)) staffName
        translate (loc,evt) = case evt of
          PedalStop   -> ( [ DebugSusPed loc PedalStop   staffName]
                         , [ toMidi t PedalStop])
          PedalStart  -> ( [ DebugSusPed loc PedalStart  staffName]
                         , [ toMidi t PedalStart])
          PedalChange -> ( [ DebugSusPed loc PedalChange staffName]
                         , [ toMidi tChange1 PedalStop
                           , toMidi tChange2 PedalStart 
                           ]
                         )
          where
            t = lookupTime loc tm
            tChange1 = t + liftDelta
            tChange2 = t + liftDelta + liftDur
    let output = mconcat . map translate . M.toList $ evts
    mapM_ tellDebugOutput . fst $ output
    return $ SusPedEventsRecord staffName (L.sort $ snd output)
