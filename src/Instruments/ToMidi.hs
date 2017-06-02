{-# LANGUAGE FlexibleContexts, TupleSections #-}

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
import Instruments.ToUnitTimeMods
import Common.CommonData
import Instruments.TimeMap
import Instruments.ApplyTimeMod
import Instruments.Dynamics
import Instruments.Curves
import Midi.MidiData
import Util.Exception


-- okay we got to figure out some of this stuff.
--
-- destination for notes, possible different midi velocities. well we are just
-- going to have a single routine for translating notes and configure it as
-- needed. more compact represenation of channels and instrumemnts. don't need
-- to specify axe in everything. don't need to put KS... use a constructor
-- instead.
--
-- okay so we have separation. should notes that aren't followed by anything
-- be truncated? Probably so and we can change that in the score.
--
-- should algorithm that does extension and truncation etc. be independent of
-- instrument? probably not. use a library


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
  let doInit (name,meta) = do
        newMeta <- commonInit meta >>= initMeta
        modify (\s -> s {tsMetas = M.insert name newMeta (tsMetas s)})
  (M.toList `liftM` gets tsMetas) >>= mapM_ doInit

  -- make time maps
  gets tsTimVar >>= makeTimeMaps

  -- make loudness curves
  makeLoudnessCurves

  -- run each meta-instrument
  let doRun (name,meta) = do
        runMeta meta begMsr endMsr
        -- modify (\s -> s {tsMetas = M.insert name newMeta (tsMetas s)})
  (M.toList `liftM` gets tsMetas) >>= mapM_ doRun

   -- this will normalize notes to start 0.1 seconds into the track
  normalize 0.1

  -- convert notes, raws, and init raws into shorts
  convertToShorts

----------------------------------------------------------------------
--                    common initialization

commonInit :: Meta -> Tr Meta
commonInit meta = do
  score <- gets tsScore
  let staffNs = mStaffNs meta
      {-
      a       staffN = (staffN, mkArticMap staffN score)
      trem    staffN = (staffN, mkTremMap staffN score)
      trill   staffN = (staffN, mkTrillMap staffN score)
      stacDur staffN = (staffN, mkStacMap staffN score)

      aMap       = M.fromList $ map a staffNs
      tremMap    = M.fromList $ map trem staffNs
      trillMap   = M.fromList $ map trill staffNs
      stacDurMap = M.fromList $ map stacDur staffNs

      arpMap = mkArpMap (getStaffNs meta) score
      -}
  
  return
    . mSetArpDelta      (mkArpMap staffNs score)
    . mSetStacDurs      (mkStacMap  score)
    . mSetTrillShapes   (mkTrillMap score)
    . mSetTremShapes    (mkTremMap  score)
    . mSetArtics        (mkArticMap score)
    . mSetTrunc         (mkTruncMap score)
    $ mSetLegExt        (mkExtMap   score) meta


----------------------------------------------------------------------


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
  notesAsShort <- (concatMap noteToShort . concat) `liftM` gets tsNotes
  rawsAsShort  <- (map rawToShort . concat) `liftM` gets tsRaws
  initRawsAsShort <- (map rawToShort . concat) `liftM` gets tsInitRaws
  return $ notesAsShort ++ rawsAsShort ++ initRawsAsShort


noteToShort :: TrNote -> [Short]
noteToShort note = concatMap (noteDestToShort (tnOnOff note)) $ tnDests note

{-
noteToShort :: TrNote -> [Short]
noteToShort note = [ Short t1 stream statusOn  p (tnVel note)
                   , Short t2 stream statusOff p 64
                   ] ++ mods
  where
    p = tnPitch note
    statusOn  = 0x90+chan-1
    statusOff = 0x80+chan-1
    (t1,t2) = headTimes $ tnOnOff note
    (stream,chan) = tnChanNums note
    mods = concatMap (toShortMod t1 t2 (tnChanNums note)) $ tnMods note
-}


noteDestToShort :: OnOff -> DestData -> [Short]
noteDestToShort onOff (DestData (stream,chan) pit vel modsData) =
    [ Short t1 stream statusOn  pit vel
    , Short t2 stream statusOff pit 64
    ] ++ mods
  where
    statusOn  = 0x90+chan-1
    statusOff = 0x80+chan-1
    (t1,t2)   = headTimes onOff
    mods = concatMap (toShortMod t1 t2 (stream,chan)) modsData


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


findNominalTimes :: OnOff -> (Double,Double)
findNominalTimes (OnOff list) = snd x
  where x = case L.find ((== "nominal") . fst) list of {Just y -> y}


makeTimeMaps :: TimingVariation -> Tr ()
makeTimeMaps tVar = do
  score <- gets tsScore
  let timeSigs = scTimeSigs score
  
  -- compute time mods
  let (globTM,staffTM) = computeUnitTimeMods score
  modify (\s -> s {tsTimeMods = staffTM})

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
      expandToVoices :: Curve -> Map Int Curve
      expandToVoices c = M.fromList $ map (,c) [1..4]
  modify (\s -> s {tsLoudness = M.map expandToVoices loudnessCurves})


tmLookup :: Ord k => k -> Map k a -> a
tmLookup k m = case M.lookup k m of {Just x -> x}



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
