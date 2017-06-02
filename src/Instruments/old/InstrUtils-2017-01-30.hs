
module Instruments.InstrUtils where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Debug.Trace
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import Data.Maybe
import Text.Printf
import Common.CommonData
import Common.CommonUtil
import Score.ScoreData
import Instruments.InstrumentsData
import Translation.TranslationData
import Translation.TranslationUtil
import Translation.TimeMap
import Util.Exception
import Midi.MidiData

----------------------------------------------------------------------
--                          'I' monad

updateValueI :: String -> Value -> I ()
updateValueI valueName v = do
  n <- name `liftM` ask
  state <- lift $ gets tsState
  let mod = modify (\s -> s {tsState = M.insert n v state})
  lift $ modify mod


----------------------------------------------------------------------
----------------------------------------------------------------------
--                 lookup loudness


loudnessAtLocTr :: String -> Int -> Loc -> Tr Double
loudnessAtLocTr staffName vn loc = do
  atm <- iuMapLookup "1" staffName `liftM` gets tsAbsTimeMaps
  lfs <- gets tsLoudFuncs
  let lf =
       iuMapLookup "2" vn . iuMapLookup "3" staffName $ lfs
  return $ lf (Left loc)


loudnessAtDblTr :: String -> Int -> Double -> Tr Double
loudnessAtDblTr staffName vn t = do
  atm <- iuMapLookup "4" staffName `liftM` gets tsAbsTimeMaps
  lf  <- (iuMapLookup "5" vn . iuMapLookup "6" staffName) `liftM` gets tsLoudFuncs
  return $ lf (Right t)
  

----------------------------------------------------------------------
----------------------------------------------------------------------
--                 utils for initialization

areElemsValid :: [String] -> [String] -> Elem -> Maybe String
areElemsValid validBrck validParam (Bracketed _ _ _ elems) =
  case snd $ L.partition pred elems of
    [] -> Nothing
    xs -> Just $ printf "invalid elems in meta config: %s"
                 (concatMap (\x -> simpleShowElem x ++ ",") xs)
  where
    pred (Bracketed _ _ t _) = t `elem` validBrck
    pred (Param _ _ t _) = t `elem` validParam
    pred (Single _ _ _) = False


defaultTrillMap :: [String] -> Map String TrillShape
defaultTrillMap staffNames = M.fromList $ map (id &&& const trill) staffNames
  where
    trill = TrillShape Upper [(14,1)] Lower


defaultStacMap :: [String] -> Double -> Map String Double
defaultStacMap stNames stac = M.fromList $ map (id &&& const stac) stNames

----------------------------------------------------------------------
----------------------------------------------------------------------
--               time- and Loc- related

onOffTr :: String -> Loc -> Loc -> Tr OnOff
onOffTr staffName l1 l2 = do
  t1 <- lookupTimeTr staffName l1
  t2 <- lookupTimeTr staffName l2
  return $ OnOff [("nominal",(t1,t2))]


noteOnOffTr :: NoteKey -> Tr OnOff
noteOnOffTr nk = onOffTr (staffNameK nk) (begLocK nk) (nkTrueEnd nk)


chordOnOffTr :: ChordKey -> Tr OnOff
chordOnOffTr ck = onOffTr (staffNameK ck) (begLocK ck) (endLocK ck)


chordOnMaxOffTr :: ChordKey -> Tr OnOff
chordOnMaxOffTr ck = onOffTr (staffNameK ck) (begLocK ck) (maxNoteEndK ck)


----------------------------------------------------------------------
----------------------------------------------------------------------
--                     translation of ChordKeys

{-

-- translateCkUtil
--
-- Helper for translating a ChordKey structured into three cases

translateCkUtil :: (MetaInstr -> ChordKey -> Tr [MidiEvent]) ->
                   (MetaInstr -> ChordKey -> Either (Int,Int) ([Int],[Int])
                              -> Tr [MidiEvent]) ->
                   MetaInstr -> ChordKey -> Tr ()
translateCkUtil doSingle doTrillTrem mi ck = do
  c <- computeCase ck
  evts <- case c of
    SingleCase       -> doSingle mi ck
    TrillCase   pits -> doTrillTrem mi ck (Left  pits)
    TremoloCase pits -> doTrillTrem mi ck (Right pits)
  prependOutputTr evts

-}

----------------------------------------------------------------------
----------------------------------------------------------------------

{-

genericChord :: Generic -> MetaInstr -> ChordKey -> Tr [MidiEvent]
genericChord generic mi ck = do
  let doNote :: NoteKey -> Tr MidiEvent
      doNote nk@(NoteKey (ChordKey staffName loc vn _) note) = do
        t1 <- noteOnOffTr nk
        t2 <- case isShort ck of
                True -> case gShortenStaccato generic of
                  True -> do let sDur = iuMapLookup staffName . cStacDur $
                                        miCommon mi
                             stacTimesTr sDur nk t1
                  False -> return t1
                False -> return t1
        t3 <- case gArp generic of
                Just staves -> arpTr staves (cArpDelta $ miCommon generic) nk
                               t2
                Nothing     -> return t2
        l <- loudnessAtLocTr staffName vn loc
        let (vMin,vMax) = gVelRange generic
        v1 <- round $ scaleClip 1 l 8 (fromIntegral vMin) (fromIntegral vMax)
        let d = gVelDelta generic
        v2 <- trRandomR (v1-d,v1+d)
        let v3 = max vMin (min vMax v2)
        return $ NoteEvent (gDest generic) t3 (gLegatoOverlap generic)
                 (gSepSame generic) (staffName,loc) (midiPitch $ nPitch note)
                  v3 []
  mapM doNote $ chordNoteKeysK ck


genericTrill :: Generic -> MetaInstr -> TrillTremPitches -> Tr [MidiEvent]
genericTrill generic mi pitches = do
  t1 <- chordOnMaxOffTr ck
  let legOvlp = 
  
-}


----------------------------------------------------------------------
----------------------------------------------------------------------
--                general-purose translation helpers


iuMapLookup :: (Show k, Ord k) => String -> k -> Map k a -> a
iuMapLookup s k m = case M.lookup k m of
  Just x -> x
  Nothing -> throwMine $ "at " ++ s ++ ", key:" ++ (show k)


computeCase :: ChordKey -> Tr TranslationCase
computeCase ck | length ds > 0 = return $ TremoloCase (ns,ds)
               | otherwise = do
                   mAlter <- maybeTrill ck
                   case mAlter of             
                     Nothing -> return $ SingleCase
                     Just x  -> return $ TrillCase $ computeTrillCase ck x
  where
    ns = map nkMidiPitch $ chordNoteKeysK ck
    ds = map nkMidiPitch $ doubTremNoteKeysK ck


-- we need
--
--   a base note, with its step and octave and midi pitch
--
--   an "upper alter" - will upper note be neutral, sharped or flatted
--
--


-- this needs upper alter, to determine chord as one single note, and pass
-- that note to compute trill pitch
computeTrillCase :: ChordKey -> Int -> (Int,Int)
computeTrillCase ck upperAlter = case chordNoteKeysK ck of
  [n] -> computeTrillPitch upperAlter n
  _   -> throwMine $ printf ("can't apply trill to chord at %s with " ++
         "multiple pitches") (showLoc2 $ begLocK ck)


-- needs upper alter, midi pitch of base note, step and octave of base note,
--
--  Returns (midi pitch, Pitch)
computeTrillPitch :: Int -> NoteKey -> (Int,Int)
computeTrillPitch upperAlter nk = (nkMidiPitch nk,newMidiPitch)
  where
    Pitch _ step _ octave = nPitch $ nkNote nk
    newStep = (step+1) `mod` 7
    newOctave = if step == 6 then octave+1 else octave
    newMidiPitch = stepAlterOctToMidi newStep upperAlter newOctave


maybeTrill :: ChordKey -> Tr (Maybe Int)
maybeTrill (ChordKey staffName loc voiceNum _) = do
  marks <- (iuMapLookup "8" staffName . scMarksByStaff) `liftM` gets tsScore
  let maybeTrill :: MarkD -> Maybe Int
      maybeTrill (SymbolMark s vn)
        | vn /= voiceNum = Nothing
        | otherwise = case s of
                        "trill-natural" -> Just 0
                        "trill-flat"    -> Just (-1)
                        "trill-sharp"   -> Just 1
                        "Trill"         -> Just 0
                        _               -> NOTHING
      maybeTrill _ = Nothing
  return $ case M.lookup loc marks of
             Nothing -> Nothing
             Just ms -> case mapMaybe maybeTrill ms of
               []  -> Nothing
               [x] -> Just x


----------------------------------------------------------------------
----------------------------------------------------------------------
arpTr :: [String] -> Double -> NoteKey -> OnOff -> Tr OnOff
arpTr staffNames delta nk onOff = do
  -- we need to find if this is marked as an arp. then need to look up its
  -- order in all arp notes here.
  let (reverseFlag,absDelta) | delta < 0 = (True, -delta)
                             | otherwise = (False, delta)
      (t1,t2) = headTimes onOff
  if S.member Arpeggiate . cModifiers . ckChord $ nkCk nk
     then do
       ordering <- arpTr2 staffNames reverseFlag nk
       return $ consTimes "arp" (t1+absDelta*fromIntegral ordering) t2 onOff
     else return onOff


-- need to figure out order of this note within list of all arpeggiated notes
-- at this Loc
arpTr2 :: [String] -> Bool -> NoteKey -> Tr Int
arpTr2 staffNames reverseFlag nk = do
  -- we need to look up all pitches on all staves
  let loc = ckChordLoc $ nkCk nk
  pitches <- concat `liftM` mapM (arpPitchesTr loc) staffNames
  let sortedPitches | reverseFlag = reverse $ L.sort pitches
                    | otherwise   = L.sort pitches
  return $ case L.elemIndex (nkMidiPitch nk) sortedPitches of
             Just i -> i
       

----------------------------------------------------------------------
----------------------------------------------------------------------
--              looking up arpeggiated pitches on a staff
--
--                     entry point is arpPitchesTr


arpPitchesTr :: Loc -> String -> Tr [Int]
arpPitchesTr loc staffName = do
  staff <- (iuMapLookup "9" staffName . scStaves) `liftM` gets tsScore
  return $ staffArpPitches_ loc staff

chordArpPitches_ :: Chord -> [Int]
chordArpPitches_ chord
  | Arpeggiate `S.member` (cModifiers chord) = map (midiPitch . nPitch) .
                                               M.elems $ cNotes chord
  | otherwise = []


-- Given a Loc and a Staff, find any chords at that Loc (in any voice
-- number), and for all the chords that are arpeggiated, list their pitches.
staffArpPitches_ :: Loc -> Staff -> [Int]
staffArpPitches_ loc staff = case M.lookup loc (stChords staff) of
  Nothing -> []
  Just cs -> concatMap chordArpPitches_ $ M.elems cs

{-

scoreArpPitches :: Loc -> Tr [Int]
scoreArpPitches loc = do
  score <- gets tsScore
  return . concatMap (staffArpPitches loc) . M.elems $ scStaves score

-}

-- arp
--
-- 
arp :: Loc -> Int -> Salue -> Double
arp _ _ _ = error "arp"

stac :: OnOff -> Salue -> OnOff
stac oo (SDouble dur) = consTimes "staccato" t1 (t1+dur) oo
  where
    (t1,t2) = headTimes oo
    

stacDur :: MetaInstr -> NoteKey -> Maybe Double
stacDur mi nk@(NoteKey (ChordKey staffName _ _ _) _)
  | isShort nk = Just dur
  | otherwise  = Nothing
  where
    dur = iuMapLookup "10" staffName . cStacDur $ miCommon mi
  


doStacTimes :: Double -> OnOff -> OnOff
doStacTimes d times = consTimes "staccato" t1 (t1+d) times
  where
    (t1,t2) = headTimes times

----------------------------------------------------------------------
----------------------------------------------------------------------



checkSlurTr :: NoteKey -> Tr Bool
checkSlurTr (NoteKey (ChordKey staffName loc vn _) _) = do
  m <- (stSlurs . iuMapLookup "11" staffName . scStaves) `liftM` gets tsScore
  let g (loc1,loc2) = loc1 <= loc && loc2 > loc
  return $ any g $ M.toAscList m

----------------------------------------------------------------------
----------------------------------------------------------------------

lookupTimeTr :: String -> Loc -> Tr Double
lookupTimeTr staffName loc = do
  atm <- tmMapLookup staffName `liftM` gets tsAbsTimeMaps
  return $ lookupTime loc atm


getBegEndTr :: NoteKey -> Tr (Double,Double)
getBegEndTr nk = do
  tm <- tmMapLookup (staffNameK nk) `liftM` gets tsAbsTimeMaps 
  return (lookupTime (begLocK nk) tm, lookupTime (nkTrueEnd nk) tm)



----------------------------------------------------------------------
----------------------------------------------------------------------
  
updateMi :: String -> (MetaInstr -> MetaInstr) -> Tr ()
updateMi miName g =
  modify (\s@TrState {tsMis = m} -> s {tsMis = M.adjust g miName m})


getLoudnessFuncsTr :: String -> Tr StaffLoudnessFunc
getLoudnessFuncsTr name = trMapLookup name `liftM` gets tsLoudFuncs


trMapLookup :: Ord k => k -> Map k a -> a
trMapLookup k m = case M.lookup k m of {Just a -> a}


getTimeMapTr :: String -> Tr AbsTimeMap
getTimeMapTr name = trMapLookup name `liftM` gets tsAbsTimeMaps


----------------------------------------------------------------------
----------------------------------------------------------------------

