
module Instruments.InstrUtils where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Debug.Trace
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map(Map)
import Data.Maybe
import Text.Printf
import Common.CommonData
import Common.CommonUtil
import Score.ScoreData
import Instruments.InstrumentsData
import Instruments.ShowInstruments
import Instruments.TimeMap
import Instruments.Curves
import Instruments.Trill
import Util.Exception
import Util.Showable
import Util.Math


----------------------------------------------------------------------
--                    lookup loudness


defaultTrillMap :: [String] -> Map String TrillShape
defaultTrillMap staffNames = M.fromList $ map (id &&& const trill) staffNames
  where
    trill = TrillShape Upper [(14,1)] Lower


defaultStacMap :: [String] -> Double -> Map String Double
defaultStacMap stNames stac = M.fromList $ map (id &&& const stac) stNames

----------------------------------------------------------------------
--                       


forOneStaff :: (Meta -> String -> Loc -> Int -> Chord -> Tr a) ->
               Meta -> String -> Int -> Int -> Tr [a]
forOneStaff f meta staffName begMsr endMsr = do
  score <- gets tsScore
  --  doLoc :: (Loc,Map Int Chord) -> Maybe [a]
  let doLoc (loc@(Loc msr _),chords)
        | msr < begMsr || msr > endMsr = return Nothing
        | otherwise = do
            let doChord (vn,chord) = f meta staffName loc vn chord
            Just `liftM` mapM doChord (M.toList chords)
  let chordMap = stChords . iuLookup "a" staffName $ scStaves score
  (concat . catMaybes) `liftM` mapM doLoc (M.toList chordMap)


forAllChords :: (String -> Loc -> Int -> Chord -> Tr a) -> Tr [a]
forAllChords f = do
  score <- gets tsScore
  let doStaff (staffName,staff) = do
        let doLoc (loc,chords) = do
              let doChord (vn,chord) = f staffName loc vn chord
              mapM doChord (M.toList chords)
        concat `liftM` mapM doLoc (M.toList $ stChords staff)
  concat `liftM` mapM doStaff (M.toList $ scStaves score)


----------------------------------------------------------------------


runLoud_common :: TrNote -> Tr TrNote
runLoud_common note = do
  let staffName = tnStaffName note
      vn        = tnVn note
      onOff     = tnOnOff note
      loc       = tnLoc note
  c <- (iuLookup "b" vn . iuLookup "c" staffName) `liftM` gets tsLoudness
  case curveLookup (onTime onOff) c of
    Nothing -> throwError $ printf ("for note at %s, staff %s, loudness " ++
      "is not defined") (showLoc2 loc) staffName
    Just x  -> return note {tnLoud = x}


lookupLoud :: TrNote -> Tr Double
lookupLoud note = do
  let staffName = tnStaffName note
      vn        = tnVn note
      onOff     = tnOnOff note
      loc       = tnLoc note
  c <- (iuLookup "b" vn . iuLookup "c" staffName) `liftM` gets tsLoudness
  case curveLookup (onTime onOff) c of
    Nothing -> throwError $ printf ("for note at %s, staff %s, loudness " ++
      "is not defined") (showLoc2 loc) staffName
    Just x  -> return x
  

computeAccent note loud
  | Accent `elem` (cModifiers $ tnChord note) = loud+1
  | Tenuto `elem` (cModifiers $ tnChord note) = loud-1
  | otherwise = loud
        

clipLoud l | l < 0.5 = 0.5
           | l > 8.5 = 8.5
           | otherwise = l


----------------------------------------------------------------------   

nominalPitsTs :: Meta -> String -> Loc -> Int -> Chord -> Tr [TrNote]
nominalPitsTs meta staffName loc vn ch@(Chord _ _ (NSingles notes)) = do
  let doNote note@(Note pitch _ trueEnd _) = do
        t1 <- lookupTimeTr staffName loc
        t2 <- lookupTimeTr staffName trueEnd
        return $ TrSingle staffName loc trueEnd vn ch note
                 (OnOff [("nominal",(t1,t2))]) [] (midiPitch $ nPitch note)
                 0 0 0
  mapM doNote $ M.elems notes
nominalPitsTs meta staffName loc vn
                     ch@(Chord _ _ (NTrill tremFlag ns1 ns2)) = do
  tNom1  <- lookupTimeTr staffName loc
  let maxTrueEnd = case map nTrueEnd (M.elems ns1 ++ M.elems ns2) of
        xs@(x:_) -> maximum xs
  tNom2  <- lookupTimeTr staffName maxTrueEnd
  -- lookup shape: need to get some kind of map
  let shapes = if tremFlag then iuLookup "d" staffName $ mTremShapes  meta
                           else iuLookup "e" staffName $ mTrillShapes meta
  shape <- case M.lookupLE loc shapes of
    Nothing -> throwError $ printf ("while looking up trill or trem " ++
      "shape at %s, there was none specified prior to this loc")
      (showLoc2 loc)
    Just (_,s) -> return s
  let timesSteps = trillTimes shape tNom1 tNom2
      n = length timesSteps
      doTimePair :: ((TrillStep,Int),(Double,Double)) -> [TrNote]
      doTimePair ((step,idx),(t1,t2)) = 
        let oo = OnOff [ ("trill/trem", (t1   ,t2   ))
                       , ("nominal"   , (tNom1,tNom2)) ]
            pitches | step == Lower = M.keys ns1
                    | otherwise     = M.keys ns2
            doPitch p = TrTrill staffName loc maxTrueEnd vn ch idx n
                        oo [] p 0 0 0
        in map doPitch pitches
  return $ concatMap doTimePair timesSteps           

{-

lookupTrillTremShape :: String -> String -> Loc -> Tr TrillShape
lookupTrillTremShape type_ staffName loc =
  withExceptT (printf ("while looking for trill/trem shape in staff '%s' at "++
    "loc '%s', ") staffName (showLoc2 loc) ++) $ do
  VLocMap m <- lk $ staffData staffName..."marks"..."trillShapes"
  case M.lookupLE loc m of
    Just (_,v) -> let VTrillShape s = v in return s
    Nothing    -> throwError "no key was <= that loc"
  
-}

iuLookupLE :: Ord k => k -> Map k a -> a
iuLookupLE k m = case M.lookupLE k m of {Just (_,v) -> v}
              

----------------------------------------------------------------------

includeNotes :: [TrNote] -> Tr ()
includeNotes evts = modify (\s -> s {tsNotes = evts:tsNotes s})


includeInitRaws :: [TrRaw] -> Tr ()
includeInitRaws evts = modify (\s -> s {tsInitRaws = evts:tsInitRaws s})


includeRaws :: [TrRaw] -> Tr ()
includeRaws evts = modify (\s -> s {tsRaws = evts:tsRaws s})


----------------------------------------------------------------------



utilEndAlter :: Meta -> Map Loc Loc -> TrNote -> Double
utilEndAlter meta slurs note = case (isStac,isSlur) of
    (True,_)      -> 0
    (False,True)  -> ext
    (False,False) -> -trunc
  where
    isStac = Staccato `elem` (cModifiers $ tnChord note)
    isSlur = case M.lookupLE (tnLoc note) slurs of
      Nothing -> False
      Just (_,endSlur) | tnLoc note < endSlur -> True
                       | otherwise            -> False
    trunc = case M.lookupLE (tnLoc note)
                 $ iuLookup "k34" (tnStaffName note) (mTrunc meta) of
      Nothing -> throwMine "needs a 'trn' score mark"
      Just (_,x) -> x
    ext = case M.lookupLE (tnLoc note)
               $ iuLookup "6901" (tnStaffName note) (mLegExt meta) of
      Nothing -> throwMine "needs a 'ext' score mark"
      Just (_,x) -> x


----------------------------------------------------------------------
--                   time- and Loc- related

onOffTr :: String -> Loc -> Loc -> Tr OnOff
onOffTr staffName l1 l2 = do
  t1 <- lookupTimeTr staffName l1
  t2 <- lookupTimeTr staffName l2
  return $ OnOff [("nominal",(t1,t2))]


lookupTimeTr :: String -> Loc -> Tr Double
lookupTimeTr staffName loc = do
  atm <- tmMapLookup staffName `liftM` gets tsTimeMaps
  return $ lookupTime loc atm


{-

loudnessI :: String -> Int -> Loc -> I Double
loudnessI staffName vn loc = do
  atm <- iuLookup staffName `liftM` (lift $ gets tsTimeMaps)
  SCurveList cs <- lk $ staffCurves ... staffName ... "loudness" ... show vn
  return $ curvesLookup atm loc cs


loudnessTI :: String -> Int -> Double -> I Double
loudnessTI staffName vn t = do
  atm <- iuLookup staffName `liftM` gets tsTimeMaps
  let sd = staffCurves...staffName..."loudnessDbl"...show vn
  flag <- lift $ exists sd
  case flag of
    True -> do
      SCurveDblList cs <- lk $ sd
      return $ curvesDblLookup atm t cs
    False -> do
      SCurveList cs <- lk $ staffCurves...staffName..."loudness"...show vn
      let cds = map (toCurveDbl atm) cs
      staffCurves...staffName..."loudnessDbl"...show vn `iSet`
        SCurveDblList cds
      return $ curvesDblLookup atm t cds

-}  

{-
  
arpOnOff delay oo =
    consTimes "arp" (t1+delay) t2 oo
  where
    (t1,t2) = headTimes oo


stacOnOff :: Value -> OnOff -> OnOff
stacOnOff (VDouble dur) oo = consTimes "staccato" t1 (t1+dur) oo
  where
    (t1,t2) = headTimes oo
-}




arpCount :: [String] -> Loc -> Int -> Tr (Int,Int)
arpCount staffNames loc pitch = do
  pitches <- concat `liftM` mapM (arpPitches loc) staffNames
  let sortedUp   = L.sort pitches
      sortedDown = reverse sortedUp
      eIdx xs = case L.elemIndex pitch xs of
        {Just idx -> idx}
  return (eIdx sortedUp,eIdx sortedDown)


arpPitches :: Loc -> String -> Tr [Int]
arpPitches loc staffName = do
  sc <- gets tsScore
  let chordPitches (Chord _ mods (NSingles ps)) = M.keys ps
      chordPitches (Chord _ mods (NTrill _ ps1 ps2)) = M.keys ps1 ++ M.keys ps2
      cp c | Arpeggiate `elem` cModifiers c = chordPitches c
           | otherwise = []
      chordLookup :: Loc -> Map Loc (Map Int Chord) -> Map Int Chord
      chordLookup loc chords = case M.lookup loc chords of
        Just cs -> cs
        Nothing -> M.empty
  return . concatMap cp . M.elems . chordLookup loc . stChords .
           iuLookup "h" staffName $ scStaves sc



----------------------------------------------------------------------
--                general-purose translation helpers


iuLookup :: (Show k, Ord k) => String -> k -> Map k a -> a
iuLookup s k m = case M.lookup k m of
  Just x -> x
  Nothing -> throwMine $ "at " ++ s ++ ", key:" ++ (show k)



----------------------------------------------------------------------
isShort :: Chord -> Bool
isShort (Chord _ mods _) = Staccato `elem` mods


----------------------------------------------------------------------

----------------------------------------------------------------------
--                  map utils

mkMapUtil :: (MarkD -> Maybe a) -> Score -> Map String (Map Loc a)
mkMapUtil fn score = M.fromList . map doStaff . M.keys $ scStaves score
  where
    marks = scMarksByStaff score
    -- doStaff :: String -> (String,Map Loc a)
    doStaff staffN = ( staffN
                     , M.mapMaybe g $ iuLookup "9175" staffN marks
                     )
    g xs = listToMaybe $ mapMaybe fn xs
  

mkArticMap :: Score -> Map String (Map Loc String)
mkArticMap = mkMapUtil maybeArtic
maybeArtic m = case m of {Artic a -> Just a; _ -> Nothing}



mkTremMap :: Score -> Map String (Map Loc TrillShape)
mkTremMap = mkMapUtil maybeTrem
maybeTrem m = case m of {TremShapeMark m -> Just m; _ -> Nothing}


mkTrillMap :: Score -> Map String (Map Loc TrillShape)
mkTrillMap = mkMapUtil maybeTrill
maybeTrill m = case m of {TrillShapeMark m -> Just m; _ -> Nothing}


mkStacMap :: Score -> Map String (Map Loc Double)
mkStacMap = mkMapUtil maybeStac
maybeStac m = case m of {StacDur d -> Just d; _ -> Nothing}


mkArpMap :: [String] -> Score -> Map Loc Double
mkArpMap staffNs score = M.unions $ map doStaff staffNs
  where
    doStaff staffN = M.mapMaybe maybeArp marks
      where marks = iuLookup "k" staffN (scMarksByStaff score)
    
maybeArp :: [Mark Double] -> Maybe Double
maybeArp ms = case [d | ArpDelta d <- ms] of
  []  -> Nothing
  x:_ -> Just x


mkTruncMap :: Score -> Map String (Map Loc Double)
mkTruncMap = mkMapUtil maybeTrunc
maybeTrunc m = case m of {Trunc d -> Just d; _ -> Nothing}


mkExtMap :: Score -> Map String (Map Loc Double)
mkExtMap = mkMapUtil maybeExt
maybeExt m = case m of {Extend d -> Just d; _ -> Nothing}

----------------------------------------------------------------------

lookupVel :: String -> Double -> VelCurve -> Int
lookupVel errMsg l (VelCurve segs) = case L.find matches segPairs of
  Nothing -> throwMine $ printf ("%s: loudness of %5.2f is not " ++
    "covered by any segments in VelCurve") errMsg l
  Just ((x1,y1),(x2,y2)) -> round $ scale x1 l x2 y1 y2
  where
    matches ((t1,_),(t2,_)) = t1 <= l && l <= t2    
    segPairs = zip segs (drop 1 segs)


----------------------------------------------------------------------

