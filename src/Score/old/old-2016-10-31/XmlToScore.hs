{-# LANGUAGE TupleSections #-}
module Score.XmlToScore where

import qualified Data.List as L
import qualified Data.Set as S
import qualified XmlDoc.XmlDocData as XD
import qualified Data.Map as M
import Debug.Trace
import Data.Array
import Data.Maybe
import Data.Map(Map)
import Data.Set(Set)
import Text.Printf
import Score.ScoreData
import Score.ParseWords(toPass1Word,toWarp,combineMacroDefns,toMarkWords
                       ,markWordsToMarks)
import XmlDoc.XmlDocExport
import XmlDoc.Process(computeXmlStaves)
import Common.CommonExport
import Util.Exception
import Util.Map


{-

do we need to set up metronome marks in XmlToScore? the other place would be
extracting them in Translation/TimeMap.hs. Do we need to pass a built tempo
map to the instruments? no. 


-}



xmlToScore :: XScore -> Score
xmlToScore xscore =
  Score 
   { scTimeSigs       = timeSigs
   , scMarks          = marks
   , scStaves         = staves
   , scUsedMsrs       = computeUsedMsrs . S.unions . map stUsedMsrs . 
                        M.elems $ staves
   }
  where
    (xMsrInfo,xmlStaves) = computeXmlStaves xscore
    timeSigs = M.map (\(IXMsrInfo _ numer denom) -> TimeSig numer denom)
               xMsrInfo
    -- Staves after 
    staves    = doMarksPass2 timeSigs .
                M.mapWithKey (computeStaff xMsrInfo timeSigs) $ xmlStaves
    marks     = M.unionsWith (++) . map stMarks . M.elems $ staves


----------------------------------------------------------------------
----------------------------------------------------------------------
--                         metronome 

{-
-- Look for metronome text on all staves
computeMetronomeMarks :: Map String (Map Loc [XMsrData]) -> Map Loc Int
computeMetronomeMarks = 
  M.fromList . mapMaybe g . lMapToList . M.unionsWith (++) . M.elems
  where
    -- convert measure data to maybe a tempo marking in quarters per minute
    g :: (Loc,XMsrData) -> Maybe (Loc,Int)
    g (loc,(XMDDirection (XDMetronome beatType bpm) _ _ _)) = case beatType of
      "quarter" -> Just (loc,bpm)
      _         -> throwMine $ printf ("don't know metronome marking of " ++
                   "beat type %s at %s") beatType (simpleShowLoc loc)
    g _ = Nothing
-}


----------------------------------------------------------------------
----------------------------------------------------------------------
--                basic staff creation function


computeStaff :: Map Int IXMsrInfo -> Map Int TimeSig -> String -> 
                Map Loc [XMsrData] -> Staff
computeStaff msrInfo timeSigs staffName xmlStaff 
  | any isSlur . concat . M.elems $ xmlStaff = 
      throwMine "can't handle slurs yet"
  | otherwise =
    Staff { stName           = staffName
          , stDynamics       = computeDynamics xmlStaff
          , stHairpins       = computeHairpins xmlStaff
          , stSlurs          = M.empty
          , stPedalEvts      = computePedalEvts xmlStaff
          , stMarks          = metAndSymbolMarks xmlStaff
          , stPass1Words     = pass1Words
          , stTrueEndsMap    = computeTrueEndsMap staffName chords
          , stUsedMsrs       = S.fromList . map msrNum . M.keys $ prelimChords
          , stChords         = chords 
          }
  where
    prelimChords = computeChords msrInfo xmlStaff
    chordEndsMap = computeChordEndsMap staffName prelimChords
    chords       = processDoubTrems . processTies chordEndsMap $ prelimChords
    pass1Words   = lMapMap toPass1Word . lMapMaybe maybeWord $ xmlStaff


isSlur (XMDNote (XNNote _ _ _ _ _ _ _ _ notations _)) =
  not $ null [x | XNSlur x _ <- notations]
isSlur _ = False


maybeWord :: XMsrData -> Maybe String
maybeWord (XMDDirection (XDWords s _) _ _ _) = Just s
maybeWord _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--                compute slur map


-- slur is a type of notation. we need to look at all notes that have slur
-- start or end, make a set of Locs of slur starts and Locs of slur
-- stops. ultimately we need to make a list of intervals that are spanned by a
-- slur in a way so that given a loc L, we can look up quickly if it is
-- spanned by a slur

data SType = SStart | SStop 
data SlurData = SlurData SType Int  -- <slur type> , <voice num>
data SlurRanges = [(Loc,Loc)]


x :: [(Loc,S)] -> [(Int,SlurRanges)]
x input =
  where
    g :: [(Loc,S)] ->  


-- given a list of slur starts and stops (in one voice only), construct a
-- SlurRanges object, 
y :: [(Loc,SlurData)] -> SlurRanges
y

----------------------------------------------------------------------
----------------------------------------------------------------------
--                compute true ends map


computeTrueEndsMap :: String -> Map Loc (Map Int Chord) -> Map Loc [NoteKey]
computeTrueEndsMap staffName = listToLMap . map (\nk -> (getTrueEnd nk,nk)) . 
                               concatMap getChordNotes .
                               getChordKeys_chords staffName


----------------------------------------------------------------------
----------------------------------------------------------------------
--                 octave shift lines


computeOctShift :: Map Loc [XMsrData] -> Map Loc OctaveLine
computeOctShift xMsrData = M.mapMaybeWithKey toLine occurrences
  where
    occurrences :: Map Loc Int
    occurrences = M.fromList . mapMaybe maybeOctShift . lMapToList $ xMsrData
    warning loc = printf ("Warning: in XML, octave-shift at %s is not " ++
                  "followed by octave-shift stop") (simpleShowLoc loc)
    toLine :: Loc -> Int -> Maybe OctaveLine
    toLine _   0 = Nothing
    toLine loc s = case M.lookupGT loc occurrences of
      Nothing         -> warning loc `trace` Nothing
      Just (endLoc,0) -> Just $ OctaveLine s endLoc
      _               -> warning loc `trace` Nothing


maybeOctShift :: (Loc,XMsrData) -> Maybe (Loc,Int)
maybeOctShift (loc,XMDDirection (XDOctaveShift s) _ _ _) = Just (loc,s)
maybeOctShift _ = Nothing


processOctaveLines :: Map Loc OctaveLine -> Map Loc (Map Int Chord) ->
                      Map Loc (Map Int Chord)
processOctaveLines lines = mapOverNotes g
  where
    g :: Loc -> Note -> Note
    g loc note = case M.lookupLE loc lines of 
      Nothing -> note
      Just (_,OctaveLine s end) | loc < end -> octShiftNote s note
                                | otherwise -> note


octShiftNote :: Int -> Note -> Note
octShiftNote n note@Note{ nPitch = nPitchIn } = note { nPitch = nPitchOut }
  where
    nPitchOut = octShiftPitch n nPitchIn


----------------------------------------------------------------------
----------------------------------------------------------------------
--                  compute pedal events


computePedalEvts :: Map Loc [XMsrData] -> Map Loc PedalEvt
computePedalEvts = fixPedalEvts . lMapMaybe g
  where
    g :: XMsrData -> Maybe PedalEvt
    g (XMDDirection (XDPedal type_ hasLine) _ _ _) = Just type_
    g _ = Nothing


-- look for bugs or weird things in the Sibelius XML pedal event export
--
-- First enforce there is only one pedal event at each location, except for
-- the following situations in which there are two pedal events:
-- 
--  (1) two pedal start at same Loc:         keep only one
--  (2) two pedal change at same Loc:        keep only one
fixPedalEvts :: Map Loc [PedalEvt] -> Map Loc PedalEvt
fixPedalEvts = M.map g
  where
    g :: [PedalEvt] -> PedalEvt
    g evts = case evts of
      [x]                        -> x
      [PedalStart , PedalStart ] -> PedalStart
      [PedalChange, PedalChange] -> PedalChange
      

----------------------------------------------------------------------
----------------------------------------------------------------------
--               compute symbols


computeSymbols :: Map Loc [XMsrData] -> Map Loc [Symbol]
computeSymbols = lMapMaybe g
  where
    g :: XMsrData -> Maybe Symbol
    g (XMDDirection (XDOtherDirection s) _ mVoice _) = case mVoice of
      Just v -> Just $ Symbol s v
    g _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--                compute text


computeText :: Map Loc [XMsrData] -> Map Loc [Text]
computeText = listToLMap . mapMaybe g . lMapToList
  where
    g (loc,x) = fmap (loc,) $ f x
    f (XMDDirection (XDWords s defy) _ _ _) = Just $ TechniqueText s
    f _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--                    functions for processing Marks


metAndSymbolMarks :: Map Loc [XMsrData] -> Map Loc [Mark]
metAndSymbolMarks msrData = 
  M.unionWith (++) (lMapMaybeWithKey maybeMetMark msrData) 
                   (lMapMaybe        maybeSymbol  msrData)


maybeMetMark :: Loc -> XMsrData -> Maybe Mark
maybeMetMark loc (XMDDirection (XDMetronome beatType bpm) _ _ _) = 
  case beatType of
    "quarter" -> Just $ SetTempo (fromIntegral bpm)
    _         -> throwMine $ printf ("don't know metronome marking of " ++
                 "beat type %s at %s") beatType (simpleShowLoc loc)
maybeMetMark _ _ = Nothing
  

maybeSymbol :: XMsrData -> Maybe Mark
maybeSymbol (XMDDirection (XDOtherDirection s) _ mVoice _) = case mVoice of
  Just v -> Just $ SymbolMark s v
maybeSymbol _ = Nothing


-- When this function is called, the following are true about the Staff:
--
--   - 'stMarks' contains Marks that are produced from metronome and symbol
--     marks
--
--   - 'stPass1Words' contains the first conversion of XML Words (to
--     Pass1Word)
doMarksPass2 :: Map Int TimeSig -> Map String Staff -> Map String Staff
doMarksPass2 timeSigs staves = M.map doStaff staves
  where
    macroDefns = combineMacroDefns . map stPass1Words . M.elems $ staves
    -- We need to add some new marks, specifically warps. Which ones were
    -- there first?
    doStaff :: Staff -> Staff
    doStaff s@Staff {stPass1Words = p1w, stMarks = marksIn} = 
      s {stMarks = M.unionWith (++) marksIn newMarks}
      where
        -- Find out where the W's are. These will be indicated in Pass1Words.
        wLocs = S.fromList . map fst . filter (\(_,p) -> isW p) . lMapToList $ 
                p1w
        -- Convert Pass1Word to MarkWord. This will involve applying the 
        -- macros, then parsing both macro and non-macro strings.
        markWords = toMarkWords macroDefns p1w
        newMarks = markWordsToMarks timeSigs wLocs markWords

        -- 
        -- doWarp loc w@MwWarpTemp{} = toWarp w timeSigs loc wLocs
        -- doWarp _   (MwMark m)     = m
        -- newMarks = lMapMap doWarp markWords


isW :: Pass1Word -> Bool
isW P1W = True
isW _    = False


----------------------------------------------------------------------
----------------------------------------------------------------------
--                  compute dynamics


computeDynamics :: Map Loc [XMsrData] -> Map Loc [Dynamic]
computeDynamics = listToLMap . mapMaybe f . lMapToList
  where
    f :: (Loc,XMsrData) -> Maybe (Loc,Dynamic)
    f (loc,XMDDirection (XDDynamics s) mOffset mVoice _)
      = case toDyn s (case mVoice of {Just v -> v}) of
         Nothing -> printf ("Warning, unknown dynamic string %s" ++
                    " at %s") s (simpleShowLoc loc) `trace` Nothing
         x -> fmap (loc,) x
    f _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--               compute haipins


-- computeHairpins
--
-- We examine <wedge> directions that are present in the staff ('xmlData'
-- represents one staff). Their marked voice and staff will be ignored, and
-- all wedges will be included regardless of what their staff and voice are
-- set to (even if the voice is set to Nothing)
-- 
computeHairpins :: Map Loc [XMsrData] -> Map Loc Hairpin
computeHairpins = M.fromList . mapMaybe pairUp . L.tails .
                  mapMaybe toWedge . lMapToList
  where
    toWedge (loc,XMDDirection (XDWedge t) _ _ _) = Just (loc,t)
    toWedge _ = Nothing
    pairUp ((loc1,WedgeCresc):(loc2,WedgeStop):_) = 
        Just (loc1, Hairpin Crescendo  loc2)
    pairUp ((loc1,WedgeDim)  :(loc2,WedgeStop):_) = 
        Just (loc1, Hairpin Diminuendo loc2)
    pairUp _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--               compute chords


computeChords :: Map Int IXMsrInfo -> Map Loc [XMsrData] -> 
                 Map Loc (Map Int Chord)
computeChords msrInfo =
  M.mapWithKey (noteMapToChordMap msrInfo) . M.map groupNotesByVoice . 
    lMapMaybe xMsrDataToNote


xMsrDataToNote :: XMsrData -> Maybe XNote
xMsrDataToNote (XMDNote n@XNNote{}) = Just n
xMsrDataToNote _ = Nothing


groupNotesByVoice :: [XNote] -> Map Int [XNote]
groupNotesByVoice = listToLMap . map (\n -> (getVoice n,n))
  where getVoice n = case XD.xnVoice n of {Just v -> v}


noteMapToChordMap :: Map Int IXMsrInfo -> Loc -> Map Int [XNote] -> 
                     Map Int Chord
noteMapToChordMap msrInfo chordBegin = M.map (notesToChord chordBegin msrInfo)


notesToChord :: Loc -> Map Int IXMsrInfo -> [XNote] -> Chord
notesToChord chordBegin msrInfo notesIn = 
  Chord endingLoc modifierData notesOut M.empty
  where
  firstNote:_ = notesIn
  endingLoc = computeEndLoc msrInfo chordBegin (XD.xnDuration firstNote)
  modifierData = S.fromList . concatMap getChordModifiers $ notesIn
  notesOut = M.fromList . zip [1..] . map toNote $ notesIn


getChordModifiers :: XNote -> [ChordModifier]
getChordModifiers n@XNNote {XD.xnNotations=notations} = concatMap g notations
  where
  g :: XNotation -> [ChordModifier]
  g (XNArticulations arts) = concatMap artToMod arts
  g (XNOrnaments orns)     = mapMaybe ornToMod orns
  g XNFermata              = [Fermata]
  g XNArpeggiate           = [Arpeggiate]
  g (XNTechnical techs)    = map techToMod techs
  artToMod XAStaccato       = [Staccato]
  artToMod XAStaccatissimo  = [Staccatissimo]
  artToMod XAAccent         = [Accent]
  artToMod XAStrongAccent   = [StrongAccent]
  artToMod XATenuto         = [Tenuto]
  artToMod XADetachedLegato = [Staccato,Tenuto]
  ornToMod (Tremolo type_ nBars) = Just $ case type_ of
    TremoloSingle  -> SingTrem      nBars
    TremoloStart   -> DoubTremStart nBars
    TremoloStop    -> DoubTremStop  nBars
  ornToMod _ = Nothing
  techToMod XTOpenString = OpenString
  techToMod XTDownBow    = DownBow
  techToMod XTUpBow      = UpBow


toNote :: XNote -> Note
toNote XNNote { XD.xnPitch = XPitch stepString alter octave
              , XD.xnTieStart = isTied } =
  Note (Pitch midiPitch step alter octave) isTied (Loc 0 0) 
    NormalHead
  where
    step = case lookup stepString [ ("C",0),("D",1),("E",2),("F",3)
                                  , ("G",4),("A",5),("B",6)] of 
      Just x -> x
    pitchClass = 
      case lookup stepString [("C",0),("D",2),("E",4),("F",5),("G",7)
                             ,("A",9),("B",11)] of
        Just x -> x
    midiPitch = (octave+1) * 12 + pitchClass + alter


-- computeEndLoc
--
-- Map Int IXMsrInfo
-- Loc : begin loc of a note
-- Int : duration of the note in number of divisions per quarter
--
computeEndLoc :: Map Int IXMsrInfo -> Loc -> Int -> Loc
computeEndLoc xmis (Loc locMsr locBeat) dur = case M.lookup locMsr xmis of
  Just (IXMsrInfo dpq numer denom)
    -- exhausted case means number of divs put end loc past beat 1 of next msr
    | e == fromIntegral numer + 1 -> Loc (locMsr+1) 1
    | e <  fromIntegral numer + 1 -> Loc locMsr e
    where
      e = locBeat + (fromIntegral denom / 4) *
          (fromIntegral dur / fromIntegral dpq)


----------------------------------------------------------------------
----------------------------------------------------------------------
--                      ties


computeChordEndsMap :: String -> Map Loc (Map Int Chord) -> Map Loc [ChordKey]
computeChordEndsMap name = listToLMap . map g . getChordKeys_chords name
  where g c = (getChordEnd c,c)


chooseNoteKeyPreferVoice :: Int -> [NoteKey] -> Maybe NoteKey
chooseNoteKeyPreferVoice voiceNum nks = case matching of
  x:_ -> Just x
  []  -> case nonMatching of
    x:_  -> Just x
    []   -> Nothing
  where 
    (matching,nonMatching) = L.partition ((==voiceNum) . getVoiceNum) nks
    

-- Set Note nTrueEnd based on the end of its tie chain, and eliminate Notes
-- that are tied to previous Notes.
processTies :: Map Loc [ChordKey] -> Map Loc (Map Int Chord) -> 
               Map Loc (Map Int Chord)
processTies chordEnds staff = removeEmpties . M.mapWithKey mapEachLoc $ staff
  where
  mapEachLoc :: Loc -> Map Int Chord -> Map Int Chord
  mapEachLoc loc = M.mapWithKey mapEachChord
    where
    mapEachChord :: Int -> Chord -> Chord
    mapEachChord voiceNum chord@Chord {cNotes=ns} =
      chord {cNotes = 
        M.mapMaybe ( processTieNote "" chordEnds staff loc 
                                    voiceNum chord
                   ) ns }


processTieNote :: String -> Map Loc [ChordKey] -> Map Loc (Map Int Chord) -> 
                  Loc -> Int -> Chord -> Note -> Maybe Note
processTieNote staffName chordEnds staffChords loc voiceNum chord note
  | isEndTie  = Nothing
  | otherwise = Just note {nTrueEnd = findTrueEnd staffChords nk}
  where
    isEndTie = case M.lookup loc chordEnds >>= findNoteMatchingPitch nk of
       Nothing -> False
       Just nk -> nIsTied . nkNote $ nk
    nk = NoteKey staffName loc voiceNum chord note


findNoteMatchingPitch :: NoteKey -> [ChordKey] -> Maybe NoteKey
findNoteMatchingPitch noteKey = 
    chooseNoteKeyPreferVoice (getVoiceNum noteKey) .
    filter ((== getMidiPitch noteKey) . getMidiPitch) . 
    concatMap getChordNotes


chordsToMatchingNote :: NoteKey -> Loc -> Map Int Chord -> Maybe NoteKey
chordsToMatchingNote nk loc = findNoteMatchingPitch nk . map g . M.toList 
  where
    g (vn,chord) = ChordKey "" loc vn chord


findTrueEnd :: Map Loc (Map Int Chord) -> NoteKey -> Loc
findTrueEnd chords noteKey
  | not (nIsTied . nkNote $ noteKey) = getChordEnd noteKey
  | otherwise = 
      let loc = getChordEnd noteKey
      in case M.lookup loc chords >>=
              chordsToMatchingNote noteKey loc of
           Nothing -> printf "Warning, no matching note follows tie at %s"
                 (simpleShowLoc $ getChordLoc noteKey) `trace` 
                 getChordEnd noteKey
           Just n  -> findTrueEnd chords n


----------------------------------------------------------------------
----------------------------------------------------------------------
--                  double tremolos


processDoubTrems :: Map Loc (Map Int Chord) -> Map Loc (Map Int Chord)
processDoubTrems staff = removeEmpties . M.mapWithKey mapEachLoc $ staff
  where
  mapEachLoc loc = M.mapMaybeWithKey (processDoubTremChord staff loc)

  
processDoubTremChord :: Map Loc (Map Int Chord) -> Loc -> Int -> Chord -> 
                        Maybe Chord
processDoubTremChord staff loc voiceNum chordIn
  | isJust $ findDoubTremStop chordIn = Nothing
  | isJust $ findDoubTremStart chordIn = 
      case M.lookup (cEndLoc chordIn) staff >>= M.lookup voiceNum of
        Nothing -> printf ("Warning, can't find chord following double "++
                   "tremolo start at %s") (simpleShowLoc loc) `trace` Nothing
        Just c -> Just $ merge chordIn c
  | otherwise = Just chordIn
  where
    merge c1@(Chord _ mods ns _)  (Chord e2 _ ns2 _) = 
      Chord e2 mods (M.map (\n -> n {nTrueEnd=e2}) ns) ns2


findDoubTremStart :: Chord -> Maybe Int
findDoubTremStart chord = 
  listToMaybe [n | DoubTremStart n <- S.toList $ cModifiers chord]


findDoubTremStop :: Chord -> Maybe Int
findDoubTremStop chord = 
  listToMaybe [n | DoubTremStop n <- S.toList $ cModifiers chord]


removeEmpties :: Map Loc (Map Int Chord) -> Map Loc (Map Int Chord)
removeEmpties = M.mapMaybe maybeRemoveLoc
  where
  maybeRemoveLoc :: Map Int Chord -> Maybe (Map Int Chord)
  maybeRemoveLoc chords
    | M.null m = Nothing
    | otherwise = Just m
    where m = M.mapMaybe maybeRemoveChord chords
  maybeRemoveChord :: Chord -> Maybe Chord
  maybeRemoveChord c | null (cNotes c) = Nothing
                     | otherwise       = Just c


----------------------------------------------------------------------
----------------------------------------------------------------------
--              Computing blank measures


computeUsedMsrs :: Set Int -> Array Int Bool
computeUsedMsrs s = listArray (1,maxMsr) (repeat False) //
                    zip (S.toList s) (repeat True)
  where
    maxMsr = maybe 1 fst (S.maxView s)
