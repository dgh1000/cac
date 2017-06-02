
module Instruments.ToMidi where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Instruments.ConfigFile as CF
import Text.Printf
import Debug.Trace
import Util.Exception
import System.Random
import Control.Monad.State
import Control.Arrow
import Data.Map(Map)
import Data.Array.IArray
import Translation.TranslationData
import Translation.ApplyTimeMod
import Translation.ToUnitTimeMods
import Translation.TimeMap
import Translation.LoudnessFunc
import Translation.AlterTimes
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Instruments.Piano
import Instruments.Q
import Midi.MidiData
import Common.CommonData
import Common.CommonUtil
import Score.ScoreData


----------------------------------------------------------------------
----------------------------------------------------------------------


-- so we need to hold state while converting notes one a time in time
-- order. should this state be part of Tr? is is possible to make a monad
-- within Tr? Tr holds the random generator state, and we need that. anything
-- else? debug output. let's stay within Tr
--
-- how should we convert continuous events? probably also along with state
--
-- we need codes at the start of a composition. what should handle producing
-- those? a meta-instrument is sending codes to certain tracks. I just
-- realized that I don't have any way to ensure that their aren't two meta
-- instruments sending to the same dests. I should catch that. so a meta instr
-- handles initial events


-- this is a function that initializes all the meta instruments
toMidi :: ConfigFile -> PlayCmd -> StdGen -> Score ->
          ([MidiEvent],[RawMidiEvent])
toMidi config cmd gen score = evts
  where
    s = TrState config cmd score gen M.empty M.empty (0,0) 
                (initializeMetaInstruments config) M.empty [] []
    (evts,finalSt) = runState toMidi2 s

-- okay so config will be type q and need to provide the config. so q has a
-- type field


initializeMetaInstruments :: ConfigFile -> Map String MetaInstr
initializeMetaInstruments (CF.ConfigFile metas _) =
    M.fromListWithKey g $ map f metas
  where
    g k _ _ = throwMine $ printf ("in config file, there are two meta-" ++
              "instruments named '%s'") k
    f :: Meta -> (String,MetaInstr)
    f me = case CF.name me of
        "piano"     -> let m = pianoInit me in (nameG m, m)
        "qSoloCello"-> let m = qInit qSoloCelloConfig me in (nameG m, m)
        s -> throwMine $ printf ("in config 'meta' data, 'instr' was " ++
             "set to '%s' but there is no meta-instrument of this type") s
          
      {-
      where
        t = runExcMsg "while searching 'meta'-type elem, " $
            findParam1 "instr" e
        
      -}
      

toMidi2 :: Tr ([MidiEvent],[RawMidiEvent])
toMidi2 = do

  -- Given command-line input, compute the measure range to translate. Right
  -- now we compute all notes, even if they are outside this range, but we
  -- only compute continuous controls within this range.
  computeMsrRange
  
  -- make time maps: this must be called before calling makeLoudnessFuncs
  makeTimeMaps

  -- make loudness functions
  makeLoudnessFuncsTr

  -- construct list of ScoreObject's in ascending time order
  scoreObjs <- makeScoreObjects

  -- translate ScoreObject's one by one
  mapM_ doScoreObject scoreObjs

  -- additional passes
  evtsOutput <- (alterTimes . concat) `liftM` gets tsOutput
  rawInits <- trackInits
  let ns = normalizeRawMidi 0.1 $ toRawMidi evtsOutput
  return (evtsOutput,rawInits++ns)


  
----------------------------------------------------------------------
----------------------------------------------------------------------

makeLoudnessFuncsTr = do
  score <- gets tsScore
  atms  <- gets tsAbsTimeMaps
  let (x,y) = makeLoudnessFuncs score atms
  modify (\s -> s {tsLoudFuncs=y, tsLoudnessDebugs=x})

----------------------------------------------------------------------
----------------------------------------------------------------------

trackInits = do
  mis <- M.elems `liftM` gets tsMis
  let f :: (Int,Int,Int,Int,Int) -> RawMidiEvent
      f (a,b,c,d,e) = MidiSingle (-0.1) a b c d e
      g :: MetaInstr -> [RawMidiEvent]
      g m = map f $ (cInitTrack $ miCommon m) m
  return $ concatMap g mis
 
      

----------------------------------------------------------------------
----------------------------------------------------------------------

{-

   JANUARY 2017, SAVE THIS

checkNonImplementedMarks = do
  -- marks :: Map String (Map Loc [Mark])
  marks <- scMarksByStaff `liftM` gets tsScore
  let f :: Loc -> [MarkD] -> [MarkD]
      f loc ms | any nonImplemented ms = throwMine $ printf
                   "unimplemented mark at %s" (showLoc2 loc)
               | otherwise = ms
      g :: Score -> Score
      g s@Score {scMarksByStaff=m} =
        s {scMarksByStaff = M.map (M.mapWithKey f) m}
      h :: TrState -> TrState
      h s@TrState {tsScore = sc} = s {tsScore = g sc}
  modify h


nonImplemented :: MarkD -> Bool
nonImplemented m@PatternMark {} = True
nonImplemented _ = False

-}

----------------------------------------------------------------------
----------------------------------------------------------------------

normalizeRawMidi :: Double -> [RawMidiEvent] -> [RawMidiEvent]
normalizeRawMidi offset evts = map (adjust $ m-offset) evts
  where
    tOf (MidiSingle x _ _ _ _ _) = x
    tOf (MidiPair   x _ _ _ _ _) = x
    m = case evts of
      [] -> error "normalizeRawMidi"
      xs -> minimum $ map tOf evts
    adjust x (MidiSingle t a b c d e) = MidiSingle (t-x) a b c d e
    adjust x (MidiPair t1 t2 b c d e) = MidiPair (t1-x) (t2-x) b c d e

----------------------------------------------------------------------
----------------------------------------------------------------------

{-
initialMidiState = do
  initialStates <- M.map miInit `liftM` getMis
  return $ MidiTranslationState confDefaultArpDelta initialStates M.empty
  

getMis :: Tr (Map String MetaInstrument)
getMis = error "implement getMis"
-}


doScoreObject :: ScoreObject -> Tr ()
doScoreObject so = do
  -- if this score object is a chord type then only translate it if it's
  -- within the measure range
  (mBeg,mEnd) <- gets tsMsrRange
  case so of
    ck@(SoCk (Loc m _) _ _ _) | mBeg <= m && m <= mEnd -> doScoreObject' ck
                              | otherwise              -> return ()
    x -> doScoreObject' x


doScoreObject' :: ScoreObject -> Tr ()
doScoreObject' so = do
  mi <- findMetaInstrument $ soStaffName so
  case so of
    SoMarks loc staffName marks ->
      mapM_ (translateMark (cName $ miCommon mi) staffName loc) marks
    SoCk _ _ _ ck -> (cTranslateCk $ miCommon mi) mi ck


findMetaInstrument :: String -> Tr MetaInstr
findMetaInstrument staffName = do
  let pred mi = staffName `elem` (cStaffNames . miCommon) mi
  result <- (L.find pred . M.elems) `liftM` gets tsMis
  case result of
    Just mi -> return mi


----------------------------------------------------------------------
----------------------------------------------------------------------

makeScoreObjects :: Tr [ScoreObject]
makeScoreObjects = do
  score <- gets tsScore
  let doCk ck@(ChordKey staffName loc vn _) = SoCk loc staffName vn ck
      doStaff :: Staff -> [ScoreObject]
      doStaff staff = map doCk $ getChordKeys_staff staff
      ms :: Map Loc [(String,[MarkD])]
      ms2 :: [(Loc,[(String,[MarkD])])]
      ms = M.map M.toList $ scMarks score
      ms2 = M.toList ms
      f :: (Loc,[(String,[MarkD])]) -> [ScoreObject]
      f (loc,staves) = map (g loc) staves
      g :: Loc -> (String,[MarkD]) -> ScoreObject
      g loc (staffName,marks) = SoMarks loc staffName marks
      ms3 :: [ScoreObject]
      ms3 = concatMap f ms2
  return . L.sort $ ms3 ++ concatMap doStaff (M.elems $ scStaves score)

----------------------------------------------------------------------
----------------------------------------------------------------------
--                   computeMsrRange


computeMsrRange :: Tr ()
computeMsrRange = do
  score <- gets tsScore
  (confBeg,maybeConfEnd) <- pcmMsrRange `liftM` gets tsPlayCmd 
  let endMsr = case maybeConfEnd of
        Just x  -> x
        Nothing -> findEndMsr score confBeg
  modify (\s -> s {tsMsrRange = (confBeg,endMsr)})


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




----------------------------------------------------------------------
----------------------------------------------------------------------


{-

transScoreObject :: ScoreObject -> Tr ()
transScoreObject
  (SoChordKey _ ck@(ChordKey staffName loc vn
                          (Chord endLoc mods notes doubTremNotes))) = do
      
  -- Find out what meta-instrument these are directed to. This is done by
  -- knowing the staff and the latest marks on it.
  metaInstr <- findMetaInstr staffName loc

  -- Send chord to meta-instr
  newNotes <- miTransChord metaInstr ck

  -- Concatenate new notes onto existing notes
  concatNewNotes newNotes

-}


-- translate notes 
--
-- so what is better to make use of similarities between instruments? have a
-- top-level routine with structure which calls into individual pieces of an
-- instrument? probably not... less flexible and honestly not that clear in
-- hindsight. or have single instrument functions which call into libraries of
-- commonly used 


-- same staccato default for all instruments? yeah




----------------------------------------------------------------------  
----------------------------------------------------------------------  
  

{-
-- functions on MetaInstr
--
translate :: MetaInstr -> ScoreObject -> Tr ()
translate mi (SoMarks loc staffName marks) =
  mapM_ (translateMark mi staffName loc) marks
translate mi (SoCk _ _ _ ck) = miTranslateCk mi mi ck
-}



-- translateMark

translateMark :: String -> String -> Loc -> MarkD -> Tr ()

translateMark _ _ _ m | not $ isTranslatedMark m = return ()

translateMark name _ _ (ArpDelta d) = updateMi name $ arpDeltaS d
  
translateMark name staffName _ (StacDur d) =
  updateMi name $ stacDurS staffName d

translateMark name staffName _ (TrillShapeMark s) =
  updateMi name $ trillShapeS staffName s 

translateMark name staffName _ (TremShapeMark s) =
  updateMi name $ tremShapeS staffName s

translateMark name staffName loc m = do
  mi <- getMi name
  (cTranslateMark $ miCommon mi) mi staffName loc m

getMi :: String -> Tr MetaInstr
getMi name = tMapLookup name `liftM` gets tsMis


tMapLookup :: Ord k => k -> Map k a -> a
tMapLookup k m = case M.lookup k m of
  Just x -> x


isTranslatedMark :: MarkD -> Bool
isTranslatedMark (InstrTechnique _)  = True
isTranslatedMark (ArpDelta _)        = True
isTranslatedMark (StacDur _)         = True
isTranslatedMark (TrillShapeMark _)  = True
isTranslatedMark (TremShapeMark _)   = True
isTranslatedMark (PatternMark _)     =
  throwMine "pattern marks aren't implemented yet"
isTranslatedMark _                   = False


----------------------------------------------------------------------
----------------------------------------------------------------------



toRawMidi :: [MidiEvent] -> [RawMidiEvent]
toRawMidi = concatMap doEvent


doEvent :: MidiEvent -> [RawMidiEvent]
doEvent (NoteEvent (stream,chan) onOff _ _ _ pitch vel modifs) =
  [MidiPair (onTime onOff) (offTime onOff) stream chan pitch vel]
  ++ map (doModif stream chan onOff) modifs
doEvent (TrillTremEvent (stream,chan) onOff _ _ _ list modifs) = map f list
  where
    f :: ((Int,Int),OnOff) -> RawMidiEvent
    f ((pit,vel),o) = MidiPair (onTime o) (offTime o) stream chan pit vel
  

doModif :: Int -> Int -> OnOff -> Modif -> RawMidiEvent
doModif stream chan onOff (Modif rt mDest evt) =
  MidiSingle t stream chan cc data1 data2
  where
    (cc,data1,data2) = case evt of {Left x -> x}
    t = case rt of
      RtOn x  -> fst (findNominalTimes onOff) + x
      RtOff x -> snd (findNominalTimes onOff) + x
    (s,c) = case mDest of {Nothing -> (stream,chan)}        


findNominalTimes :: OnOff -> (Double,Double)
findNominalTimes (OnOff list) = snd x
  where x = case L.find ((== "nominal") . fst) list of {Just y -> y}



----------------------------------------------------------------------
----------------------------------------------------------------------
--                        time computations


makeTimeMaps :: Tr ()
makeTimeMaps = do
  -- compute time mods
  timeMods <- computeUnitTimeMods
  modify (\s -> s {tsUnitTimeMods=timeMods})
  ratio <- pcmTempoRatio `liftM` gets tsPlayCmd
  -- make base time map
  mTv   <- cfTVar `liftM` gets tsConfigFile
  score <- gets tsScore
  let base = computeBaseTimeMap score mTv ratio timeMods
  staffs <- M.map toAbsolute `liftM` applyStaffTimeMods base timeMods
  modify (\s -> s {tsAbsTimeMaps=staffs})


-- 
computeUnitTimeMods :: Tr [UnitTimeMod]
computeUnitTimeMods = do
  score <- gets tsScore
  let marks   = scMarksByStaff score
      markers = scMarkers score
      allStNa = M.keys $ scStaves score
      ts      = scTimeSigs score
      context = Context allStNa ts markers
      f :: (Context -> Map String (Map Loc [MarkD]) -> [UnitTimeMod]) ->
           [UnitTimeMod]
      f g     = g context marks
  return $ f doWarps ++ f doAbsWarps ++ f doPauses ++ f doRitPauses ++
           f doMultPauses


-- apply staff time mods
applyStaffTimeMods :: RelTimeMap -> [UnitTimeMod] -> Tr (Map String RelTimeMap)
applyStaffTimeMods baseM timeMods = do
  ts <- scTimeSigs `liftM` gets tsScore
  stNames  <- (M.keys. scStaves) `liftM` gets tsScore
  let -- allMatching :: [[UnitTimeMod]]
      -- allMatching name = map (filter (matches name) timeMods) $ staffNames
      matches name (UnitWarp maybeName _ _) = case maybeName of
        Just n | n == name -> True
               | otherwise -> False
        Nothing -> False
      matches _ _ = False
      doStaff :: String -> RelTimeMap
      doStaff s = foldl (applyTimeMod ts) baseM $ filter (matches s) timeMods
  return . M.fromList $ map (id &&& doStaff) stNames



