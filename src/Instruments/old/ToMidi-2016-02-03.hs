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
import Data.Map(Map)
import Score.ScoreData
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Common.CommonData
import Translation.TranslationData
import Translation.TimeMap
import Translation.AlterTimes
import Midi.MidiData
import Translation.ApplyTimeMod
import Translation.Dynamics
import Translation.Curves
import Util.Exception

-- toMidi
--
--   - initialize
toMidi :: (Int,Maybe Int) -> Tr [RawMidiEvent]
toMidi (begMsr,mEndMsr) = do

  -- find end measure
  
  endMsr <- case mEndMsr of
    Nothing -> gets tsScore >>= return . findEndMsr begMsr
    Just x  -> return x
  
  -- initialize each meta
  let runInit i = runReaderT (initialize i) i
  gets tsMetas >>= mapM runInit . M.elems

  -- make time maps
  gets tsTimVar >>= makeTimeMaps

  -- make loudness curves
  makeLoudnessCurves

  -- construct list of ScoreObjects in ascending time order
  scoreObjs <- makeScoreObjects

  -- translate ScoreObjects one by one
  mapM_ (doScoreObject begMsr endMsr) scoreObjs

  -- do time alteration and normalizing of final events
  finalizeEvents
  

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

  let curveMap = hpDynCurves score
      insertCurve staffName curve vn = do
        let xForm (SCurveList cs) = SCurveList $ curve:cs
        trXformDefault (staffCurves ... staffName ... "loudness" ... show vn)
                       xForm (SCurveList [curve])
      doStaff (staffName,curve) = mapM (insertCurve staffName curve) [1..4]
  mapM_ doStaff $ M.toList curveMap


-- make a list of score object in ascending Loc order, with all objects at
-- same Loc being sorted alphabetically by staff name
makeScoreObjects :: Tr [ScoreObject]
makeScoreObjects = do
  score <- gets tsScore
  let stavePairs = M.toList $ scStaves score

  let doStaff (staffName,staff) = concatMap doLoc $ M.toList $ stChords staff
        where
          doLoc (loc,chordMap) = map doVn $ M.keys chordMap
            where
              doVn vn = SoCk loc staffName vn
      -- okay now we have to get marks
      doStaff2 (staffName,staff) = concatMap doLoc $ M.toList $ scMarks score
        where
          doLoc (loc,markMap) = map doName $ M.toList markMap
            where
              doName (name,marks) = SoMarks loc name marks

  return . L.sort $ concatMap doStaff  stavePairs ++
                    concatMap doStaff2 stavePairs



doScoreObject :: Int -> Int -> ScoreObject -> Tr ()
doScoreObject begMsr endMsr so = case so of
  s@(SoCk loc@(Loc msr _) staffName vn)
    | begMsr <= msr && msr <= endMsr -> doScoreObject2 s
    | otherwise -> return ()
  s -> doScoreObject2 s


-- doScoreObject2
--
-- Knowing that ScoreObject is either Marks or within the measure range,
-- translate it
doScoreObject2 :: ScoreObject -> Tr ()
doScoreObject2 so = do
  let f meta = soStaffName so `elem` staves meta
  x <- (L.find f . M.elems) `liftM` gets tsMetas
  case x of
    Just theMeta -> runReaderT (translate theMeta so) theMeta
    Nothing -> throwMine $ printf ("in doScoreOjbect2, staff name '%s' does"++
               " not belong to any meta instr") (soStaffName so)


tmLookup :: Ord k => k -> Map k a -> a
tmLookup k m = case M.lookup k m of {Just x -> x}



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
--                 finding measure range

findEndMsr :: Int -> Score -> Int
findEndMsr begMsr score
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

doChordNominalPitsTs :: ChordContent -> Tr [TrNote]
doChordNominalPitsTs (ChordContext staffName loc vn
                   ch@(Chord _ _ (NSingles notes))) = do
  let doNote note@(Note pitch _ trueEnd _) = do
        t1 <- lookupTime staffName loc
        t2 <- lookupTime staffName loc
        TrSingle staffName loc trueEnd vn ch note
          (OnOff [("nominal",(t1,t2))]) (0,0) (midiPitch $ nPitch note) 0
  mapM doNote notes
doChordNominalPitsTs (ChordContext staffName loc vn
                      ch@(Chord _ _ (NTrill tremFlag ns1 ns2))) = do
  atm <- tmLookup staffName `liftM` gets tsTimeMaps
  t1  <- lookupTime staffName loc
  let maxTrueEnd = case map nTrueEnd (M.elems ns1 ++ M.elems ns2) of
        xs@(x:_) -> maximum xs
  t2  <- lookupTime staffName maxTrueEnd
  -- lookup shape: need to get some kind of map
  VMarkMap trMap <-
    lk $ staffData..."markMaps"...(if tremFlag then "tremShapes"
                                               else "trillShapes")
  let VTrillShape shape = tmLookupLE loc trMap
  (n,timesSteps) = trillTimes shape t1 t2
  let doTimePair :: ((TrillStep,Int),(Double,Double)) -> [TrNote]
      doTimePair ((step,idx),(t1,t2)) = 
        let oo = OnOff [ ("trill/trem",(t1,t2))
                       , ("nominal",(onTime tNom,offTime tNom)) ]
            pitches | step == Lower = M.keys ns1
                    | otherwise     = M.keys ns2
            doPitch p = TrTrill staffName loc maxTrueEnd vn chord idx n
                        oo (0,0) p 0
        in map doPitch pitches
  return $ concatMap doTimePair timesSteps           


doDest_piano :: TrNote -> Tr TrNote
doDest_piano note = do
  VMap destMap <- lk $ metaData..."dests"
  return $ note {tnDest = pLookup (tnStaffName note) destMap}

doDest_qFourChans :: TrNote -> Tr TrNote
doDest_qFourChans note = do
  
  -- compute dest string name. refer to articulation mark map and choose
  -- the LE one; or choose "short" if chord has short modifier

  VMap artMap <- lk $ metaData..."markMaps"..."articulation"
  let theArt = qLookupLE (tnLoc note) m
  destName | isShort (tnChord note) = "short"
           | otherwise              = let VArt s = theArt in s

  -- look up dest in dest 

  VMap dests <- lk $ staffData...tnStaffName..."dests"
  return note {tnDest = qLookup destName dests}

doModif_qKeyswitch :: TrNote -> Tr TrNote
doModif_qKeyswitch note = do
  -- we are computing keyswitch modifiers there will be
  VLocMap artMap <- lk $ staffData..."art"
  art <- qLookupLE (tnLoc note) 
  let ksName | isShort (tnChord note) = "short"
             | otherwise = let VInt p = 
  VStringMap ksMap <- lk $ metaData..."keyswitchMap"
  let m = ModifKs (Left (-0.05)) (qLookup ksName ksMap)
  case note of
    TrSingle{} -> note {tnMods = m:tnMods note}
    TrTrill{}
      | tnCount note == 0 -> note {tnMods = m:tnMods note}
      | otherwise         -> note

