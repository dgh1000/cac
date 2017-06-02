{-# LANGUAGE FlexibleContexts #-}

module Instruments.ToMidi where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Array
import Debug.Trace
import Text.Printf
import Data.Either
import System.Random
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map(Map)
import Score.ScoreData
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Common.CommonData
import Translation.TranslationData
import Translation.TimeMap
import Translation.ApplyTimeMod
import Translation.Dynamics
import Translation.Curves
import Midi.MidiData
import Util.Exception

-- toMidi
--
--   - initialize
toMidi :: (Int,Maybe Int) -> Tr [Short]
toMidi (begMsr,mEndMsr) = do

  -- find end measure
  
  endMsr <- case mEndMsr of
    Nothing -> gets tsScore >>= return . findEndMsr begMsr
    Just x  -> return x
  
  -- initialize each meta
  let runInit meta = initMeta meta (metaName meta) (metaStaffNames meta)
  gets tsMetas >>= mapM_ runInit

  -- make time maps
  gets tsTimVar >>= makeTimeMaps

  -- make loudness curves
  makeLoudnessCurves

  -- run each meta-instrument
  let doRunMeta meta = runMeta meta (metaName meta) (metaStaffNames meta)
                       begMsr endMsr
  gets tsMetas >>= mapM_ doRunMeta

   -- this will normalize notes to start 0.1 seconds into the track
  normalize 0.1

  -- convert notes, raws, and init raws into shorts
  convertToShorts


normalize delay = do
  notes <- concat `liftM` gets tsNotes
  newNotes <- case notes of
    [] -> throwError "in normalize, no notes present"
    xs -> let tMin = minimum $ map getOnTime xs
          in return $ map (offsetOne (-tMin+delay)) notes
  modify (\s -> s {tsNotes = [newNotes]})
  

getOnTime :: TrNote -> Double
getOnTime = onTime . tnOnOff


offsetOne :: Double -> TrNote -> TrNote
offsetOne d note = note {tnOnOff = new}
  where
    new = consTimes (printf "offset %.3f" d) (t1+d) (t2+d) (tnOnOff note)
    (t1,t2) = headTimes $ tnOnOff note


----------------------------------------------------------------------
--                   convert TrNote and TrRaw to shorts


convertToShorts = do
  notesAsShort <- concatMap noteToShort `liftM` gets tsNotes
  rawsAsShort  <- map rawToShort `liftM` gets tsRaws
  initRawsAsShort <- map rawToShort `liftM` gets tsInitRaws
  return $ notesAsShort ++ rawsAsShort ++ initRawsAsShort


noteToShort :: TrNote -> [Short]
noteToShort note = [ Short t1 stream statusOn  p (tnVel note)
               , Short t2 stream statusOff p 64
               ] ++ mods
  where
    p = tnPitch note
    statusOn  = 0x90+chan-1
    statusOff = 0x80+chan-1
    (t1,t2) = headTimes $ tnOnOff note
    (stream,chan) = tnDest note
    mods = concatMap (toShortMod t1 t2 (tnDest note)) $ tnMods note


toShortMod :: Double -> Double -> (Int,Int) -> Modif -> [Short]
toShortMod t1 t2 (stream,chan) (ModifKs timing key) =
    [ Short tOn  stream (0x90+chan-1) key 64
    , Short tOff stream (0x80+chan-1) key 64 ]
  where
    tOn = case timing of
      Left d -> t1+d
    tOff = tOn + 0.05

rawToShort :: TrRaw -> Short
rawToShort (TrRaw _ t (stream,chan) status data1 data2)
  = Short t stream (status+chan-1) data1 data2


-- okay we need to translate all these events to hard midi. this requires
-- knowing final times. doing short. doing end time alteration. normalizing
-- most events, and adding in track start events

{-

finalizeEvents :: Tr [RawMidiEvent]
finalizeEvents = do
  regularEvts <- concat `liftM` gets tsEvts
  metaState ... "regularEvents" `trSet` SEvents regularEvts
  let raws1 = normalizeRawMidi 0.1 .
              concatMap toRawMidi . alterTimes $ regularEvts
  metaState ... "rawRegularEvents" `trSet` SRawEvents raws1
  initEvts <- concat `liftM` gets tsInitEvts
  metaState ... "initEvts" `trSet` SEvents initEvts
  let raws2 = concatMap toRawMidi initEvts
  metaState ... "rawInitEvts" `trSet` SRawEvents raws2
  return $ raws1 ++ raws2



toRawMidi :: MidiEvent -> [RawMidiEvent]
toRawMidi (NoteEvent (stream,chan) onOff _ _ _ pitch vel modifs) =
  [MidiPair (onTime onOff) (offTime onOff) stream chan pitch vel]
  ++ map (doModif stream chan onOff) modifs
toRawMidi (TrillTremEvent (stream,chan) onOff _ _ _ list modifs) = map f list
  where
    f :: ((Int,Int),OnOff) -> RawMidiEvent
    f ((pit,vel),o) = MidiPair (onTime o) (offTime o) stream chan pit vel
toRawMidi (RawEvent e) = [e]


doModif :: Int -> Int -> OnOff -> Modif -> RawMidiEvent
doModif stream chan onOff (Modif rt mDest evt) =
  MidiSingle t stream chan cc data1 data2
  where
    (cc,data1,data2) = case evt of {Left x -> x}
    t = case rt of
      RtOn x  -> fst (findNominalTimes onOff) + x
      RtOff x -> snd (findNominalTimes onOff) + x
    (s,c) = case mDest of {Nothing -> (stream,chan)}        

-}

findNominalTimes :: OnOff -> (Double,Double)
findNominalTimes (OnOff list) = snd x
  where x = case L.find ((== "nominal") . fst) list of {Just y -> y}


makeTimeMaps :: TimingVariation -> Tr ()
makeTimeMaps tVar = do
  score <- gets tsScore
  let timeSigs = scTimeSigs score
  
  -- compute time mods
  let (globTM,staffTM) = computeUnitTimeMods score

  let base = computeBaseTimeMap score tVar 1 globTM

  let doStaff :: String -> (String,RelTimeMap)
      doStaff staffName = case M.lookup staffName staffTM of
        Nothing   -> (staffName,base)
        Just mods -> (staffName,foldl (applyTimeMod timeSigs) base mods)
      staffTMs = M.map toAbsolute $
                 M.fromList $ map doStaff (M.keys $ scStaves score)
  modify (\s -> s {tsTimeMaps=staffTMs})


makeLoudnessCurves :: Tr ()
makeLoudnessCurves = do
  score <- gets tsScore
  atms  <- gets tsTimeMaps
  -- we need to store curves in staff stateB
  let loudnessCurves = hpDynCurves atms score
      storeOneStaff (staffName,curve) = do
        let doOneVoice vn = staffData staffName..."loudness"...show vn
                            `trSet` VCurve curve
        mapM_ doOneVoice [1..4]
  mapM_ storeOneStaff $ M.toList loudnessCurves


tmLookup :: Ord k => k -> Map k a -> a
tmLookup k m = case M.lookup k m of {Just x -> x}


{-
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
-}

----------------------------------------------------------------------
--                 finding measure range

findEndMsr :: Int -> Score -> Int
findEndMsr begMsr score
  | begMsr > (snd $ bounds a) =
      throwMine ("Given beginning measure is past "++
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

doDest_qFourChans :: String -> TrEvent -> Tr TrEvent
doDest_qFourChans metaName note = do
  
  -- compute dest string name. refer to articulation mark map and choose
  -- the LE one; or choose "short" if chord has short modifier

  VLocMap artMap <- lk $ metaData metaName... "markMaps"..."articulation"
  let VString theArt = qLookupLE (tnLoc note) artMap
      destName | isShort (tnChord note) = "short"
               | otherwise              = theArt

  -- look up dest in dest 

  VMap dests <- lk $ staffData tnStaffName note..."dests"
  let VDest d = qLookup destName dests
  return note {tnDest = d}

doModif_qKeyswitch :: String -> TrEvent -> Tr TrEvent
doModif_qKeyswitch metaName note = do
  -- we are computing keyswitch modifiers there will be
  VLocMap artMap <- lk $ staffData...tnStaffName note..."art"
  let VString currentArt = qLookupLE (tnLoc note) artMap
      ksName | isShort (tnChord note) = "short"
             | otherwise = currentArt
  VMap ksMap <- lk $ metaData...metaName..."keyswitchMap"
  let VInt ksNum = qLookup ksName ksMap
  let m = ModifKs (Left (-0.05)) ksNum
      noteWithMods = note {tnMods = m:tnMods note}
  case note of
    TrSingle{} -> return noteWithMods
    TrTrill{}
      | tnCount note == 0 -> return noteWithMods
      | otherwise         -> return note

-}



{-

qLookup_VInt :: String -> Map String Value -> Int
qLookup_VInt key m = case M.lookup key m of
  Just (VInt i) -> i
-}

