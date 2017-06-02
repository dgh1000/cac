{-# LANGUAGE TupleSections #-}
module Score.XmlToScore where

import qualified Data.List as L
import qualified Data.Set as S
import qualified XmlDoc.XmlDocData as XD
import qualified Data.Map as M
import Control.Arrow
import Debug.Trace
import Data.Array
import Data.Maybe
import Data.Map(Map)
import Data.Set(Set)
import Text.Printf
import Score.ScoreData
import Score.ParseMarks(computeWordMarks,combineMacroDefns)
import XmlDoc.XmlDocExport
import XmlDoc.Process(computeXmlStaves)
import Common.CommonExport
import Util.Exception
import Util.Map
import Common.CommonUtil


xmlToScore :: XScore -> Score
xmlToScore xscore =
  Score 
   { scTimeSigs       = timeSigs
   , scMarks          = allMarks
   , scMarksByStaff   = marksByStaff
   , scMarkers        = M.map computeMarkers marksByStaff
   , scStaves         = staves
   , scUsedMsrs       = computeUsedMsrs . S.unions . map stUsedMsrs . 
                        M.elems $ staves
   }
  where
    (xMsrInfo,xmlStaves) = computeXmlStaves xscore
    timeSigs = M.map (\(IXMsrInfo _ numer denom) -> TimeSig numer denom)
               xMsrInfo
    -- xmlStaves is Map String (Map Loc [XMsrData]). 
    stavesWords :: Map String (Map Loc [String])
    stavesWords = M.map (lMapMaybe maybeWord) xmlStaves
    
    -- we need to compute Map Loc (Map String [Mark]
    
    allMarks'    = computeWordMarks stavesWords
    staves       = M.mapWithKey (computeStaff xMsrInfo timeSigs) $ xmlStaves
    allMarks     = foldInMetSymbolMarks staves allMarks'
    marksByStaff = computeMarksByStaff (M.keys staves) allMarks




----------------------------------------------------------------------
----------------------------------------------------------------------
--                   mark computations

computeMarksByStaff :: [String] -> Map Loc (Map String [MarkD]) ->
                       Map String (Map Loc [MarkD])
computeMarksByStaff names mIn = foldl f (flipMap mIn) names
  where
    f m name | M.member name m = m
             | otherwise       = M.insert name M.empty m



foldInMetSymbolMarks :: Map String Staff -> Map Loc (Map String [MarkD]) ->
                        Map Loc (Map String [MarkD])
foldInMetSymbolMarks staves marksIn =
    foldl merge marksIn . map g $ M.toAscList staves
  where
    g :: (String,Staff) -> Map Loc (Map String [MarkD])
    g (staffName,staff) = M.map (M.singleton staffName) $ stMetSymMarks staff
    merge :: Map Loc (Map String [MarkD]) -> Map Loc (Map String [MarkD]) ->
             Map Loc (Map String [MarkD])
    merge = M.unionWith (M.unionWith (++))


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


-- 



-- computeStaff
--
--   marks
--
--     computing them requires two phases
--
--       goal of phase 1: gather macro definitions from all staves. this is
--       done by translation to Pass1Word first, which will pull out macro
--       definitions and instances but not translate any other marks
--
--       goal of phase 2: change 
--
--
--  problem: some marks will be specific to a staff, while some will be paired
--  with marks which are on a different staff
-- 
computeStaff :: Map Int IXMsrInfo -> Map Int TimeSig -> String -> 
                Map Loc [XMsrData] -> Staff
computeStaff msrInfo timeSigs staffName xmlStaff =
    Staff { stName           = staffName
          , stDynamics       = computeDynamics xmlStaff
          , stHairpins       = computeHairpins xmlStaff
          , stPedalEvts      = computePedalEvts xmlStaff
          , stMetSymMarks    = metSym
          , stTrueEndsMap    = M.empty {- computeTrueEndsMap staffName chords -}
          , stUsedMsrs       = S.fromList . map msrNum . M.keys $ prelimChords
          , stSlurs          = computeSlurs xmlStaff
          , stChords         = chords 
          }
  where
    metSym = metAndSymbolMarks xmlStaff
    prelimChords = computeChords msrInfo xmlStaff
    chordEndsMap = computeChordEndsMap prelimChords
    chords       = prelimChord2Chord metSym $
                   processTies chordEndsMap prelimChords
    -- pass1Words   = lMapMap toPass1Word . lMapMaybe maybeWord $ xmlStaff



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


{-


data SType = SStart | SStop 
data SlurData = SlurData SType Int  -- <slur type> , <voice num>
data SlurRanges = [(Loc,Loc)]


x :: [(Loc,S)] -> [(Int,SlurRanges)]
222222222222222222222x input =
  where
    g :: [(Loc,S)] ->  


-- given a list of slur starts and stops (in one voice only), construct a
-- SlurRanges object, 
y :: [(Loc,SlurData)] -> SlurRanges
y


-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                compute true ends map

{-

computeTrueEndsMap :: String -> Map Loc (Map Int Chord) -> Map Loc [NoteKey]
computeTrueEndsMap staffName = listToLMap . map (\nk -> (nkTrueEnd nk,nk)) . 
                               concatMap chordNoteKeysK .
                               getChordKeys_chords staffName
-}

-- we need way to generate all note keys


{-
                xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

                         JANUARY 2017

           not sure what true ends map used for, lets leave it out


computeTrueEndsMap :: Map Loc (Map Int Chord) -> Map Loc [StaffNoteKey]
computeTrueEndsMap chords = listToLMap xs
  where
    xs = concatMap g $ M.toList chords
    g :: (Loc,Map Int Chord) -> [(Loc,StaffNoteKey)]
    g (loc,m) = map (h loc) (M.toList m)
    h :: Loc -> (Int,Chord) -> [(Loc,StaffNoteKey)]
    h loc (vn,ch) = map (\(p,end) -> (end,StaffNoteKey loc vn p)) $
                    chordTrueEnds ch 



-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                 octave shift lines


{-

                    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

                        January 28, 2107 LEAVING OUT OCTAVE LINES



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


-}

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



-- SlurData: <flag if any unnumbered slur starts here>
--           <the numbers of any numbered slur starts here>
--           <flag if any unnumbered slur stops here>
--           <the numbers of any numbered slur stops here>
data SlurData = SlurData Bool (Set Int) Bool (Set Int)
              deriving (Show)


computeSlurs :: Map Loc [XMsrData] -> Map Loc Loc
computeSlurs xs = M.fromList $ mapMaybe g ts
  where
    m = computeSlurs2 xs
    ts = L.tails $ M.toAscList m
    g :: [(Loc,SlurData)] -> Maybe (Loc,Loc)
    g [] = Nothing
    g ((loc,SlurData f1 s1 _ _):remain)
        | f1 && S.null s1          =
            Just (loc,seekStopUnnumbered remain)
        | f1 && not (S.null s1) = throwMine "XmlToScore:computeSlurs"
        | not f1 && length s1 == 1 =
            Just (loc,seekStopNumbered (S.elemAt 0 s1) remain)
        | otherwise = Nothing
        where
          seekStopUnnumbered :: [(Loc,SlurData)] -> Loc
          seekStopUnnumbered zs = case L.find (isStopUnnumbered . snd) zs of
            Just (loc2,_) -> loc2
          isStopUnnumbered (SlurData _ _ f2 _) = f2
          seekStopNumbered n zs = case L.find (isStopNumbered n . snd) zs of
            Just (loc2,_) -> loc2
          isStopNumbered n (SlurData _ _ _ s2) = S.member n s2
      

computeSlurs2 :: Map Loc [XMsrData] -> Map Loc SlurData
computeSlurs2 = M.mapMaybe g
  where
    g ::[XMsrData] -> Maybe SlurData
    g xs = f d
      where
        ys = concatMap maybeSlurs xs
        isStartNoNum  (flag,Nothing) = flag
        isStartNoNum  (_   ,Just _ ) = False
        isStopNoNum   (flag,Nothing) = not flag
        isStopNoNum   (_   ,Just _ ) = False
        maybeStartNum (flag,Just n ) = if flag then Just n else Nothing
        maybeStartNum (_   ,Nothing) = Nothing
        maybeStopNum  (flag,Just n ) = if flag then Nothing else Just n
        maybeStopNum  (flag,Nothing) = Nothing
        d = SlurData (any isStartNoNum ys)
                     (S.fromList $ mapMaybe maybeStartNum ys)
                     (any isStopNoNum ys)
                     (S.fromList $ mapMaybe maybeStopNum ys)
        f s@(SlurData f1 s1 f2 s2) | not f1 && not f2 &&
                                     S.null s1 && S.null s2 = Nothing
                                   | otherwise = Just s



maybeSlurs :: XMsrData -> [(Bool,Maybe Int)]
maybeSlurs (XMDNote (XNNote _ _ _ _ _ _ _ _ notations _)) =
    mapMaybe maybeNSlur notations
  where
    maybeNSlur :: XNotation -> Maybe (Bool,Maybe Int)
    maybeNSlur (XNSlur s mi) | s == "start" = Just (True ,mi)
                             | s == "stop"  = Just (False,mi)
    maybeNSlur _ = Nothing
maybeSlurs _ = []

maybeSlur (XMDNote (XNNote _ _ _ _ _ _ _ _ notations _)) =
  not $ null [x | XNSlur x _ <- notations]
isSlur _ = False




----------------------------------------------------------------------
----------------------------------------------------------------------
--                    functions for processing Marks


metAndSymbolMarks :: Map Loc [XMsrData] -> Map Loc [MarkD]
metAndSymbolMarks msrData = 
  M.unionWith (++) (lMapMaybeWithKey maybeMetMark msrData) 
                   (lMapMaybe        maybeSymbol  msrData)


maybeMetMark :: Loc -> XMsrData -> Maybe MarkD
maybeMetMark loc (XMDDirection (XDMetronome beatType bpm) _ _ _) =
  error $ "in XmlToScore.hs:maybeMetMark, not currently processing " ++
          "metronome marks"
{-  
  case beatType of
    "quarter" -> Just . SetTempo $ NumVar (fromIntegral bpm) Nothing
    _         -> throwMine $ printf ("don't know metronome marking of " ++
                 "beat type %s at %s") beatType (simpleShowLoc loc)
-}
maybeMetMark _ _ = Nothing
  

maybeSymbol :: XMsrData -> Maybe MarkD
maybeSymbol (XMDDirection (XDOtherDirection s) _ mVoice _) = case mVoice of
  Just v -> Just $ SymbolMark s v
maybeSymbol _ = Nothing


{-

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


-}

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
                 Map Loc (Map Int PrelimChord)
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
                     Map Int PrelimChord
noteMapToChordMap msrInfo chordBegin = M.map (notesToChord chordBegin msrInfo)


notesToChord :: Loc -> Map Int IXMsrInfo -> [XNote] -> PrelimChord
notesToChord chordBegin msrInfo notesIn = 
    PrelimChord endingLoc modifierData notesOut
  where
  firstNote:_ = notesIn
  endingLoc = computeEndLoc msrInfo chordBegin (XD.xnDuration firstNote)
  modifierData = S.fromList . concatMap getChordModifiers $ notesIn
  -- notesOut = M.fromList . zip [1..] . map toNote $ notesIn
  notesOut = map toNote notesIn


getChordModifiers :: XNote -> [ChordModifier]
getChordModifiers n@XNNote {XD.xnNotations=notations} = concatMap g notations
  where
  g :: XNotation -> [ChordModifier]
  g (XNArticulations arts) = concatMap artToMod arts
  g (XNOrnaments orns)     = mapMaybe ornToMod orns
  g XNFermata              = [Fermata]
  g XNArpeggiate           = [Arpeggiate]
  g (XNTechnical techs)    = map techToMod techs
  g (XNSlur _ _)           = []
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


{-

         replaced JANUARY 28

computeChordEndsMap :: String -> Map Loc (Map Int Chord) -> Map Loc [ChordKey]
computeChordEndsMap name = listToLMap . map g . getChordKeys_chords name
  where g c = (chordEndK c,c)

-}


computeChordEndsMap :: Map Loc (Map Int PrelimChord) -> Map Loc [(Loc,Int)]
computeChordEndsMap m = listToLMap $ concatMap g $ M.toList m
  where
    g :: (Loc,(Map Int PrelimChord)) -> [(Loc,(Loc,Int))]
    g (atLoc,m) = map (\(i,p) -> (prcEndLoc p,(atLoc,i))) $ M.toList m

{-

           removed JANUARY 28

chooseNoteKeyPreferVoice :: Int -> [NoteKey] -> Maybe NoteKey
chooseNoteKeyPreferVoice voiceNum nks = case matching of
  x:_ -> Just x
  []  -> case nonMatching of
    x:_  -> Just x
    []   -> Nothing
  where 
    (matching,nonMatching) = L.partition ((==voiceNum) . voiceNumK) nks

-}

-- ties first or double tremolo first? ties could eliminate an entire
-- chord. oh we did that with remove empties

{-

           replaced JANUARY 28

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

-}


-- processTies
--
-- Set true end of tied notes, and eliminate notes and chords that are
-- at the far end of ties.
--

processTies :: Map Loc [(Loc,Int)] -> Map Loc (Map Int PrelimChord) ->
               (Map Loc (Map Int PrelimChord))
processTies chordEnds staff = M.mapMaybeWithKey processLoc staff
  where
    processLoc :: Loc -> Map Int PrelimChord -> (Maybe (Map Int PrelimChord))
    processLoc loc = mmwk processChord
      where
        processChord :: Int -> PrelimChord -> Maybe PrelimChord
        processChord vn ch@(PrelimChord end _ ns) =
          let set_NSingles ns = ch {prcNotes=ns}
              f :: Note -> Maybe [Note]
              f note = (:[]) <$> (doTieNote chordEnds staff loc vn end note)
          in set_NSingles <$> (mconcat $ map f ns)


{-        
        processChord vn ch@(Chord end _ (NSingles m)) =
          let set_NSingles m = ch {cNotes=NSingles m}
          in set_NSingles <$> mmwk (doTieNote chordEnds staff loc vn end) m 
        processChord vn ch@(Chord end _ (NTrill tf a1 b1)) =
          let f m = mmwk (doTieNote chordEnds staff loc vn end) m
          in case (f a1,f b1) of
               (Just a2,Just b2) -> Just ch {cNotes = NTrill tf a2 b2}
               -- if case match failure occurs here, it means that eliminating
               -- tie notes caused one or both sides of a tremolo or trill
               -- to be eliminated
-}

                 
mmwk :: Ord k => (k -> a -> Maybe b) -> Map k a -> Maybe (Map k b)
mmwk f m | M.null m2 = Nothing
         | otherwise = Just m2
  where m2 = M.mapMaybeWithKey f m


{-

          removed JANUARY 28

processTieNote :: String -> Map Loc [ChordKey] -> Map Loc (Map Int Chord) -> 
                  Loc -> Int -> Chord -> Note -> Maybe Note
processTieNote staffName chordEnds staffChords loc voiceNum chord note
  | isEndTie  = Nothing
  | otherwise = Just note {nTrueEnd = findTrueEnd staffChords nk}
  where
    isEndTie = case M.lookup loc chordEnds >>= findNoteMatchingPitch nk of
       Nothing -> False
       Just nk -> nIsTied . nkNote $ nk
    nk = NoteKey (ChordKey staffName loc voiceNum chord) note

-}


--
-- Transform a note based on tie rules.
--
--   1. If this note is precedded by a tie, get rid of it (return Nothing)
--
--   2. Otherwsie, try to compute true end of it.
--
-- How we tell if a note, of 'atLoc' and 'vn', is preceeded by a tie?
--
--   Lookup 'atLoc' in the chord end map: that will give us [(Loc,Int)]
--
--   Those are a bunch of start locs of chords and voice numbers in the staff
--   data. Look up each of those chords, and look at each of those notes for a
--   tied note of the same pitch in any voice
--
--
--
--   True end of noteIn: (needed going into this, the chord end of the note
--                        and the voice number)
--
--   1. if the note has no tie, then the chord end is the true end
--
--   2. if the note is tied, then we begin THE HUNT for a note that follows it
--
--      a. take chord end of noteIn, and look up all chords at that Loc, with
--         or withou matching voice number
--
--      b. put all chords in order with the ones with the matching number at
--         the head of the line
--
--      c. search for first note that matches in pitch. note it's chord end
--         loc and voice number
--
--      d. go to 1
doTieNote :: Map Loc [(Loc,Int)] -> Map Loc (Map Int PrelimChord) ->
             Loc -> Int -> Loc -> Note -> Maybe Note
doTieNote chordEnds staff atLoc vnIn end note
  | isEndTie  = Nothing
  | otherwise = Just note {nTrueEnd = findTrueEnd staff end vnIn note}
  where
    -- to determine if the follows a tie and therefore should be eliminated,
    -- look it up in the chordEnds map
    m1 :: Maybe [(Loc,Int)]
    m1 = M.lookup atLoc chordEnds
    -- helpful function to order these chords so that vnIn comes first
    m2 :: Maybe [(Loc,Int)]
    m2 = myOrdering2 vnIn <$> m1
    -- function that changes (Loc,Int) to chord
    f1 :: (Loc,Int) -> PrelimChord
    f1 (l,i) = case M.lookup l staff >>= M.lookup i of
      Just c -> c
    m3 :: Maybe [PrelimChord]
    m3 = map f1 <$> m2
    -- helper fn. to break a PrelimChord into notes filters to match pitch
    pitchMatch = midiPitch $ nPitch note
    f2 :: PrelimChord -> [Note]
    f2 (PrelimChord _ _ ns) = filter ((==pitchMatch) . midiPitch . nPitch) ns
    m4 :: Maybe [Note]
    m4 = concatMap f2 <$> m3
    isEndTie = case m4 of
      Just ns -> any nIsTied ns
      Nothing -> False

{-
                                              
    -- now lookup the chord matching [(theLoc,vn)] in staff
    m2 :: Maybe PrelimChord
    m2 = m1 >>= \(l,v) -> M.lookup l staff >>= M.lookup v
    -- now lookup a note
    isEndTie = case M.lookup atLoc chordEnds >>= lookupStaff >>= lookupNote of
      Nothing   -> False
      Just note -> nIsTied note
    lookupStaff (l,vn) = M.lookup l staff >>= lookupNote . M.toList
      where
        noteMatchingPitch :: (Int,PrelimChord) -> Maybe Note
        lookupNote :: [(Int,PrelimChord)] -> Maybe Note
        lookupNote cs =
          L.find ((==midiIn) . midiPitch . nPitch) $
          concatMap (M.elems . prcNotes . snd) $ myOrdering midiIn cs
-}


{-
class PrelimNoteKey = PrelimNoteKey Loc Int Chord Note


findNote_staff ::  -> Map Loc (Map Int Chord) ->
                  Maybe PrelimNoteKey

-}

myOrdering :: Eq a => a -> [(a,b)] -> [(a,b)]
myOrdering x list = xs++ys
  where
    (xs,ys) = L.partition ((==x) . fst) list


myOrdering2 :: Eq b => b -> [(a,b)] -> [(a,b)]
myOrdering2 x list = xs++ys
  where
    (xs,ys) = L.partition ((==x) . snd) list


{-

         removed JANUARY 28

findNoteMatchingPitch :: NoteKey -> [ChordKey] -> Maybe NoteKey
findNoteMatchingPitch noteKey = 
    chooseNoteKeyPreferVoice (voiceNumK noteKey) .
    filter ((== nkMidiPitch noteKey) . nkMidiPitch) . 
    concatMap chordNoteKeysK





chordsToMatchingNote :: NoteKey -> Loc -> Map Int Chord -> Maybe NoteKey
chordsToMatchingNote nk loc = findNoteMatchingPitch nk . map g . M.toList 
  where
    g (vn,chord) = ChordKey "" loc vn chord

-}





-- findTrueEnd
--
--   we are HUNTING the TRUE end of a little Note. Upon calling this We have
--
--     the Loc at which to look next
--
--     the voice number to hunt (but we'll consider others)
--
--     the pitch to math
--
--
-- NOTES: we are givn (1) the Loc at which to hunt, (2) a
--   SUGGESTION of the voice num, and the PITCH to match. Our job it to find
--   a note
--
--   what we plan on returning is the true end, the end of the furthest note
--   along this chain we can get
--
--   basically just search, taking care to search the suggested voice num
--   first
--
--
--   An input note is passed. If this not is NOT tied to a following note,
--   then the passed chord end is returned as the true end. If it IS tied,
--   then we try to find the note at the far end of the tie, in the same voice
--   if possible, but also accepting a note in another voice. If there is no
--   such note, we return the passed chord end as the true end. However, if
--   there is such a note, we call findTrueEnd on that note.
--
--   Map Loc (Map Int Chord) :: staff chords
--   Loc  :: chord end of input note
--   Int  :: voice number of input note
--   Note :: input note
-- 

findTrueEnd _ _ _ _ = error "foo"

{-
findTrueEnd :: Map Loc (Map Int PrelimChord) -> Loc -> Int -> Note -> Loc
findTrueEnd staff end vn noteIn 
  | not (nIsTied noteIn) = end
  | otherwise = case maybeNote of
      Nothing -> printf "Warning, no matching note follows tie at %s"
                 (showLoc2 end) `trace` end
      Just (vnOut,n) -> findTrueEnd staff end vnOut n
  where
    midiIn = midiPitch $ nPitch noteIn
    -- Look in the staff at 'chordEnd' for (Map Int Chord). Look among all
    -- those Chords for a note matching pitch 'midiIn', preferring the one
    -- at voice 'vn' if it exists.
    a :: Maybe (Map Int PrelimChord)
    a = M.lookup end staff
    b :: Maybe [(Int,PrelimChord)]
    b = a >>= return . myOrdering vn . M.toList
    c :: Maybe [(Int,Note)]
    c = b >>= return . concatMap (\(v,c) -> map (v,) (prcNotes c))
    maybeNote :: Maybe (Int,Note)
    maybeNote = c >>= L.find (\(v,n) -> midiIn == (midiPitch $ nPitch n))
   
-}
----------------------------------------------------------------------
----------------------------------------------------------------------
--         PrelimChord -> Chord (adding double tremeolos and trills)


-- so dismiss/alter/ preserve. map over with maybes and elimination? decent
-- plan.


--   (Map Int PrelimChord) -> Map Int PrelimChord
--    x                    =  Nothihg
-- See x is the evil system of a double tremolo. She doesn't get preserved.
--
--    y                     = Y
--  x got bigger because she got all of x's not
--
-- call source cord (for taking pitches into the other one THE SOURCE IS WITH YOU)
--
-- cca dest cord the DESTINATION, MAN.
--
-- they are in diffent parts of the strucutre, not easily found togethier. if

-- could they be searched at the same time. 

                     

----------------------------------------------------------------------
----------------------------------------------------------------------
--                  double tremolos

-- to convert PrelimChord to Chord, doing trills at same time
--
-- basically map through each PrelimChord, changing to Chord or maybe
-- eliminating it
--
-- what do we do to decide what should be done with a PrelimChord?
--
--   first see if it's marked as a doub tremolo start
--
--     if so, we need to find the chord that ends it, grab those pitches and
--     include them.
--
--   or consider if it's a doub trem stop. then remove it
--
--   if none of the above, then check if it's a trill, and evaluate pitches if
--   so


-- we creat a function that takes a Loc and a sub-prelim chord, and converts
-- it to a sub-chord. voila! now just run this over everything and we'll be
-- done.
--
-- problem is broken down by mapping with key over sub-chords at dififerent
-- Locs. so the setup is ~~~~ you have a Loc ~~~~> and you get the submap,
-- Maybe
--
--    this is function pc2c_loc. now we can pass that over ever key in our big
--    map
--
--    so pc2c_loc has to work on the Ints of sub-maps with the help of
--
--       pc2c_vn: here now that we receiving the vn as an input, we can
--       finally full idified ctbwo: chord ctobwo, or called c1 donw there.
--
--          easy cases: if it's a marked double tremolo stop, don't sound it,
--          remove it from the score.
--
--          next easy case: set up a Trill


prelimChord2Chord :: Map Loc [MarkD] -> Map Loc (Map Int PrelimChord) ->
                     Map Loc (Map Int Chord)
prelimChord2Chord symbols staff = M.mapMaybeWithKey pc2c_loc staff
  where
    -- what does this need to do its job? 'staff' to look up doub
    -- tremolos. it's going to take a chord with some notes including a true
    -- end, and then look at a certain place for a matching chord with a doub
    -- trem stop setting. same voice
    pc2c_loc :: Loc -> (Map Int PrelimChord) -> Maybe (Map Int Chord)
    pc2c_loc atLoc = mmwk pc2c_vn
      where
        pc2c_vn :: Int -> PrelimChord -> Maybe Chord
        pc2c_vn vn c1
           | isJust $ findDoubTremStart c1 = pc2c_dt_case staff atLoc vn c1
           | isJust $ findDoubTremStop c1  = Nothing
           | otherwise =
              case isTrill symbols atLoc vn of
               Nothing  -> Just $ pc2c_main_case c1
               Just trN -> pc2c_trill_case atLoc trN c1


pc2c_main_case :: PrelimChord -> Chord
pc2c_main_case (PrelimChord end mods ns) =
  Chord end mods (NSingles $ M.fromList $ map (nmp &&& id) ns)
  where
    nmp = midiPitch . nPitch
                     


pc2c_trill_case :: Loc -> Int -> PrelimChord -> Maybe Chord
pc2c_trill_case atLoc upperAlter c1 =               
   let msg = printf ("something wrong; a trill chord has more "++
             "than one note at %s") (showLoc2 atLoc)
       note1 = case prcNotes c1 of
                 [x]  -> x
                 _    -> throwMine msg
       pitch1 = nPitch note1
       midi1 = midiPitch pitch1
       (midi2,pitch2) = computeTrillPitch upperAlter pitch1
       note2 = note1 {nPitch=pitch2}
       new1 = M.fromList [(midi1,note1)]
       new2 = M.fromList [(midi2,note2)]
   in Just $ Chord (prcEndLoc c1) (prcModifiers c1) (NTrill False new1 new2)


pc2c_dt_case staff al vn c1 = error "fpp"

                                   
computeTrillPitch :: Int -> Pitch -> (Int,Pitch)
computeTrillPitch upperAlter (Pitch _ step1 alter1 octave1) = (newMidi,pitch2)
  where
    step2 = (step1+1) `mod` 7
    octave2 = if step1 == 6 then octave1+1 else octave1
    newMidi = stepAlterOctToMidi step2 upperAlter octave2
    pitch2 = Pitch newMidi step2 upperAlter octave2



{-
pc2c_trill_case :: Loc -> PrelimChord ->



pc2c_dt_case :: Map Loc (Map Int PrelimChord) -> Loc -> Int -> PrelimChord ->
                Maybe Chord
pc2c_dt_case staff atLoc vn (PrelimChord end mods notes) = _
  where
    nmp = midiPitch . nPitch
    examTrueEnd = case notes of
      n:_ -> nTrueEnd n
    -- look for chord at this locationi
    case M.lookup examTrueEnd staff >>= M.lookup vn of
      Nothing -> throwMine $ 
      Just c2 | isJust $ findDoubTremStop c2 -> msg
                  let notes1 = M.fromList . mpa (nmp &&& id) $ notes
                      notes2 = M.fromList . map (nmp &&& id) $ prcNotes c2
                  in Just $ Chord end mods (NTrill False notes1 notes2)
              | otherwise -> msg
      where msg = printf ("can't find second tremolo chord " ++
                          "corresponding to first chord at %s")
                          (showLoc2 atLoc)
                   
-}


-- isTrill
--
-- This will compute the pitch of the "upper" trill note if there
-- is a trill mark here
--
--    it also p
isTrill :: Map Loc [MarkD] -> Loc -> Int -> Maybe Int
isTrill syms atLoc vn =
    M.lookup atLoc syms >>= listToMaybe . mapMaybe (mTrill vn)
  

mTrill :: Int -> MarkD -> Maybe Int
mTrill vn (SymbolMark s vTest) | vTest /= vn = Nothing
                               | otherwise = case s of
                                   "trill-natural" -> Just 0
                                   "trill-flat"    -> Just (-1)
                                   "trill-sharp"   -> Just 1
                                   "Trill"         -> Just 0
                                   _               -> Nothing


{-

    removed FEBRUARY 28

processDoubTrems :: Map Loc (Map Int Chord) -> Map Loc (Map Int Chord)
processDoubTrems staff = removeEmpties . M.mapWithKey mapEachLoc $ staff
  where
  mapEachLoc loc = M.mapMaybeWithKey (processDoubTremChord staff loc)


okay this takes "map with maybe" and empty later. I'd like to compute the two
choirds

-}

-- case study: what does it take to find a pair of tremolo


{-

-- what does processDoubTrems need to do it's work?
--
-- - 
processDoubTrems :: Map Loc [MarkD] -> Map Loc (Map Int PrelimChord) ->
                    Map Loc (Map Int Chord)                    
processDoubTrems syms staff = removeEmpties . M.mapWithKey mapEachLoc $ staff
  where
  -- so these cats are trasnforming each loc, with the key for it. at the loc
  -- is a group of chords and note in various voices. we further break it down
  -- here with 'mapEachLoc' definition which maps on each chord, by way of
  -- letting it know the Loc and voice numbers to 'processDoubTremChord'
    
  mapEachLoc loc = M.mapMaybeWithKey (processDoubTremChord staff loc)


-- 

-- this will take PrelimChord but will actually output Chord
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
-}

findDoubTremStart :: PrelimChord -> Maybe Int
findDoubTremStart chord = 
  listToMaybe [n | DoubTremStart n <- S.toList $ prcModifiers chord]


findDoubTremStop :: PrelimChord -> Maybe Int
findDoubTremStop chord = 
  listToMaybe [n | DoubTremStop n <- S.toList $ prcModifiers chord]

{-

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

-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--              Computing blank measures


computeUsedMsrs :: Set Int -> Array Int Bool
computeUsedMsrs s = listArray (1,maxMsr) (repeat False) //
                    zip (S.toList s) (repeat True)
  where
    maxMsr = maybe 1 fst (S.maxView s)


----------------------------------------------------------------------
----------------------------------------------------------------------
--                   compute Markers


computeMarkers :: Map Loc [MarkD] -> Markers
computeMarkers marks = Markers (s isW) (s isCD) (s isDC)
  where
    s pred = S.fromList . map fst . filter f $ M.toList marks
      where
        f (loc,ms) = any pred ms


isW :: MarkD -> Bool
isW W = True
isW _ = False


isCD EndCrescDescr = True
isCD _  = False


isDC EndDescrCresc = True
isDC _  = False


