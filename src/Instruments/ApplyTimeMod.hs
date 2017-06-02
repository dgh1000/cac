
module Instruments.ApplyTimeMod where

import qualified Data.Map as M
import Debug.Trace
import Text.Printf
import Control.Monad
import Control.Monad.State
import Control.Arrow
import Data.Map(Map)
import Data.Ratio
import Util.Math(scale)
import Util.Map(splitInclude)
import Common.CommonData
import Common.CommonUtil
import Util.Exception
import Score.ScoreData
import Instruments.InstrumentsData

----------------------------------------------------------------------
----------------------------------------------------------------------


-- applyTimeMod
--
applyTimeMod :: Map Int TimeSig -> RelTimeMap -> UnitTimeMod ->
                RelTimeMap

{-

--                       --> UnitRamp <--
applyTimeMod timeSigs (RelTimeMap tm) (UnitRamp loc1 loc2 r1 r2) =
  RelTimeMap $ M.mapWithKey g tm
  where
    rampLen = fromRational $ locDiffQuar timeSigs loc1 loc2 
    sliceLen = 1%fromIntegral slicesPerBeat
    loc2End = case locAdd timeSigs loc2 sliceLen of {Just x -> x}
    g :: Loc -> Double -> Double
    g locMid d | locMid < loc1 || locMid >= loc2End = d
               | otherwise = d*scale 0 x rampLen (1/r1) (1/r2)
      where
        x = fromRational $ locDiffQuar timeSigs loc1 locMid

-}

-- to apply a pause, we need to blow up the highest slice that ends at or
-- before the pause loc. if pause loc is msr 2 beat 1, then the slice loc we
-- need is
--                  ---> UnitPause <---
applyTimeMod timeSigs r@(RelTimeMap tm) (UnitPause loc dur) =
  RelTimeMap $ M.mapWithKey g tm
  where
    locP = findPauseLoc tm loc
    durS = secPerQuarter timeSigs r loc * fromRational dur
    g locIn d | locIn == locP = d+durS
              | otherwise     = d
                
--               ----> UnitWarp  <---
applyTimeMod timeSigs rtm (UnitWarp _ eLocs deltaQ) =
  case eLocs of
    Left (loc1,loc2) ->
      let (loc1O,loc2O) = makeOrdered loc1 loc2
      in  applyTimeMod_oneWarp timeSigs loc1O loc2O (d loc1) rtm
    Right (loc1,loc2,loc3) ->
      let warpAmt = d loc2
      in applyTimeMod_oneWarp timeSigs loc1 loc2 warpAmt  .
         applyTimeMod_oneWarp timeSigs loc2 loc3 (-warpAmt)  $ rtm
  where
    d :: Loc -> Double
    d l = secPerQuarter timeSigs rtm l * fromRational deltaQ



{-

--              --> UnitRitAccel <--

-- this will ramp tempo ratio from 1 to rat1, then jump to 1/rat1 and back to
-- 1, then scale everything so total sum is the same
applyTimeMod timeSigs (RelTimeMap tm) (Unit2Modify loc1 loc2 loc3 rat1) =
    RelTimeMap $ foldl (\m (l,d) -> M.insert l d m) tm $
                 map (second (*finalRatio)) segs1M ++
                 map (second (*finalRatio)) segs2M
  where
    l1 = sliceFloor loc1
    l2 = sliceFloor loc2
    l3 = sliceFloor loc3
    segs = M.toAscList tm
    segs1 :: [(Int,(Loc,Double))]
    segs1 = zip [0..] $ filter (\(l,_) -> l1 <= l && l < l2) segs
    segs2 = zip [0..] $ filter (\(l,_) -> l2 <= l && l < l3) segs
    n1    = length segs1
    n2    = length segs2
    sumF f xs = case sum $ map f xs of
      y | y > 0 -> y
    segs1_sum = sumF (snd . snd) segs1
    segs2_sum = sumF (snd . snd) segs2
    segs1M = map mult1 segs1
    segs2M = map mult2 segs2
    segs1M_sum = sumF snd segs1M
    segs2M_sum = sumF snd segs2M

    finalRatio = (segs1_sum+segs2_sum)/(segs1M_sum+segs2M_sum)

    -- mult: given (<seg #>,(<seg loc>,<currDur>)
    mult1 :: (Int,(Loc,Double)) -> (Loc,Double)
    mult1 (idx,(l,d)) = (l,d * scale 0 (fromIntegral idx) (fromIntegral n1)
                                     1 (1/rat1)
                        )

    mult2 (idx,(l,d)) = (l,d * scale 0 (fromIntegral idx) (fromIntegral n1)
                                     rat1 1
                        )
-}

--                   --> Unit2Modify <--
applyTimeMod timeSigs (RelTimeMap tm)
  (Unit2Modify loc1 loc2 loc3 tm1 tm2 mNormalize) = case mNormalize of
    Just norm -> let origSum = sum1+sum2
                     newSum = sumM1+sumM2
                     targetSum = (origSum-newSum)*norm + newSum
                     ratio | newSum > 0 = targetSum/newSum
                 in RelTimeMap $ tmInsertRange tm
                               $ map (second (*ratio)) together
    Nothing -> RelTimeMap $ tmInsertRange tm together 
  where
    l1 = sliceFloor loc1
    l2 = sliceFloor loc2
    l3 = sliceFloor loc3
    (n1,sum1,range1) = tmSplitOutRange l1 l2 tm
    (n2,sum2,range2) = tmSplitOutRange l2 l3 tm
    (sumM1,range1M) = applyTempoModify n1 range1 tm1 
    (sumM2,range2M) = applyTempoModify n2 range2 tm2
    together = range1M ++ range2M
    


--                  --> UnitAdjust <--
applyTimeMod timeSigs (RelTimeMap tm) (UnitAdjust _ loc1 loc2 loc3 deltaQ) =
    RelTimeMap $ tmInsertRange tm $ map (second (*rat2)) together
  where
    l1 = sliceFloor loc1
    l2 = sliceFloor loc2
    l3 = sliceFloor loc3
    (n1,sum1,range1) = tmSplitOutRange l1 l2 tm
    (n2,sum2,range2) = tmSplitOutRange l2 l3 tm
    (sumM1,range1M) = applyTempoModify n1 range1 $
                      TmRamp 1 (fromRational $ 1-deltaQ)
    -- compute target sum for range1
    deltaSec = let x = secPerQuarter timeSigs (RelTimeMap tm) loc2
               in x*(fromRational deltaQ)
    -- 
    targSum1 = sum1+deltaSec
    rat1  = targSum1/sumM1
    range1MM = let x = map (second (*rat1)) range1M
               in x
      
    -- want to restore 
    rat2 = (sum1+sum2)/(targSum1+sum2)
    together = range1MM ++ map snd range2
      


--                ---> UnitAbsWarp <--
applyTimeMod timeSigs r@(RelTimeMap tm) (UnitAbsWarp locA locB durTargetQ) =
  RelTimeMap $ M.mapWithKey g tm
  where
    (loc1,loc2) | locA < locB = (locA,locB)
                | locA > locB = (locB,locA)
    -- durC is original duration between loc1 and loc2 in seconds
    durC = case filter (\(k,_) -> k >= loc1 && k < loc2) . M.toList $ tm of
      [] -> throwMine $printf ("absolute warp between %s and %s: these locs "++
            "are too close together; no slices are included in this range")
            (showLoc2 loc1) (showLoc2 loc2)
      xs -> sum $ map snd xs

    -- durOrigQ is origina duration in quarters
    durOrigQ = durC/secPerQuarter timeSigs r locA

    -- ratio: what we need to multiply each slice by
    ratio = fromRational durTargetQ/durOrigQ

    -- function that does the alteration
    g k v | k >= loc1 && k < loc2 = v*ratio
          | otherwise             = v
    

{-
applyTimeMod_oneWarp :: TimeSigs -> Loc -> Loc -> Double -> RelTimeMap ->
                        RelTimeMap
applyTimeMod_oneWarp timeSigs loc1 loc2 amt (RelTimeMap tm) =
  RelTimeMap $ M.mapWithKey g tm  
  where
    -- compute duration of all slices in the region between loc1  loc2
    durC = case filter (\(k,_) -> k >= loc1 && k < loc2) . M.toList $ tm of
      [] -> throwMine $ printf ("warp between %s and %s: these locs are too "++
            "close together; no slices are included in this range")
            (showLoc2 loc1) (showLoc2 loc2)
      xs -> sum $ map snd xs
    -- compute final duration
    durF = durC+amt
    ratio | durF > 0 = durF/durC
          | otherwise = throwMine $ printf ("the warp at %s would result " ++
                        "in zero or negative final duration")
                        (showLoc2 loc1)
    g k v | k >= loc1 && k < loc2 = v*ratio
          | otherwise             = v
-}


-- applyTimeMod_oneWarp
--
applyTimeMod_oneWarp :: TimeSigs -> Loc -> Loc -> Double -> RelTimeMap ->
                        RelTimeMap
applyTimeMod_oneWarp timeSigs loc1 loc2 amt (RelTimeMap tm) =
  RelTimeMap $ foldr (uncurry M.insert) tm slices_pass1
  where
    inBounds l = loc1 <= l && l <= loc2
    slices_pass0 = filter (inBounds . fst) $ M.toAscList tm
    sum_pass0 = sum $ map snd slices_pass0
    sum_target | sum_pass0+amt <= 0 = throwMine "ppkj3278"
               | otherwise = sum_pass0+amt
    ratio = sum_target/sum_pass0
    slices_pass1 = map (second (*ratio)) slices_pass0
          
    


makeOrdered :: Ord a => a -> a -> (a,a)
makeOrdered x y | x <= y     = (x,y)
                | otherwise  = (y,x)
    



----------------------------------------------------------------------

sliceFloor :: Loc -> Loc
sliceFloor (Loc msr beat) = Loc msr $ floor (beat*(spb%1)) % spb
  where
    spb = fromIntegral slicesPerBeat :: Integer


----------------------------------------------------------------------
----------------------------------------------------------------------
--                   find a delay that is expressed in quarters
--              


-- Look up slice that is equal to or before input Loc, I think, to find
-- duration of one slice. Then find next slice loc, then find difference in
-- quarters and in seconds.
--
{-
deltaQToTime :: TimeSigs -> RelTimeMap -> Loc -> Rational -> Double
deltaQToTime ts (RelTimeMap tm) locIn deltaQ =
  deltaQ*sliceTime/fromRational sliceQuar
  where
    (loc1,sliceTime) = case M.lookupLE locIn tm of {Just x -> x}
    loc2 = case M.lookupGT locIn tm of {Just (x,_) -> x}
    sliceQuar = locDiffQuars ts loc1 loc2
-}

----------------------------------------------------------------------

secPerQuarter :: TimeSigs -> RelTimeMap -> Loc -> Double
secPerQuarter ts (RelTimeMap tm) locIn =
  d*fromIntegral slicesPerBeat*fromIntegral denom/4
  where
    (Loc msr _,d) = case M.lookupLE locIn tm of {Just x -> x}
    TimeSig _ denom = case M.lookup msr ts of {Just x -> x}


----------------------------------------------------------------------
----------------------------------------------------------------------
--                     useful utilities

    
findPauseLoc :: Map Loc Double -> Loc -> Loc
findPauseLoc m loc = case M.toDescList . fst . M.split locFloor $ m of
    (locP,_):_ -> locP
  where
    locFloor = case M.lookupLE loc m of {Just (x,_) -> x}


-- tmSplitOutRange

tmSplitOutRange :: Loc -> Loc -> Map Loc Double ->
                   (Int,Double,[(Int,(Loc,Double))])
tmSplitOutRange loc1 loc2 tm =
    (length items, sum $ map snd items, zip [1..] items)
  where
    items = M.toAscList $ snd $ splitInclude loc1 $ fst $ M.split loc2 tm
    


tmInsertRange :: Map Loc Double -> [(Loc,Double)] -> Map Loc Double
tmInsertRange = foldr (\(l,d) m -> M.insert l d m)


applyTempoModify :: Int -> [(Int,(Loc,Double))] -> TempoModify ->
                    (Double,[(Loc,Double)])
applyTempoModify n items tmod = (sum $ map snd segs, segs)
  where
    segs = map (\(i,(l,d)) -> (l,d/f i)) items
    f idx = case tmod of
      TmRamp r1 r2 -> scale 1 (fromIntegral idx) (fromIntegral n) r1 r2
      TmRampParab r1 r2 False ->
        scale 1 (fromIntegral idx**2) (fromIntegral n**2) r1 r2
      TmRampParab r1 r2 True ->
        scale (fromIntegral n**2) (fromIntegral (n+1-idx)**2) 1 r1 r2
