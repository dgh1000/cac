{-# LANGUAGE TupleSections #-}
module Instruments.Dynamics where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Common.CommonData as CD
import Control.Monad.State
import Control.Arrow
import Debug.Trace
import Text.Printf
import Data.Maybe
import Data.Map(Map)
import Data.Ratio
import Common.CommonData
import Common.CommonUtil
import Score.ScoreExport
import Score.ScoreData
import Instruments.TimeMap
import Instruments.Curves
import Instruments.InstrumentsData
import Util.Exception
import Util.Math(scale)



----------------------------------------------------------------------
----------------------------------------------------------------------


-- is there any reason to use int map like this or pairs to express loudness
-- func for different voices? or use a code system in curve names? The thing
-- about code system is that it unifies curves, they all get stored in the
-- same way. of course there is no reason it has to be done this way. no harm
-- in specializing the loudness curves


hpDynCurves :: Map String AbsTimeMap -> Score -> Map String Curve
hpDynCurves atms score = M.map f $ scStaves score
  where
    f :: Staff -> Curve
    f staff = let atm = dLookup (stName staff) atms
              in Curve [hpDynCurve (scTimeSigs score) atm staff]


dLookup :: Ord k => k -> Map k a -> a
dLookup k m = case M.lookup k m of {Just x -> x}


hpDynCurve :: TimeSigs -> AbsTimeMap -> Staff -> OneCurve
hpDynCurve timeSigs atm
           Staff { stHairpins    = hairpins
                 , stDynamics    = dynamics
                 , stMaxTrueEnd  = eMax } =
    foldl step (doDynamics timeSigs atm eMax dynamics) $ M.toAscList hairpins
  where
    -- we need to change this to use 
    step :: OneCurve -> (Loc,Hairpin) -> OneCurve
    step (OneCurve c ts tm) (locH1,Hairpin _ locH2) =
      let rH1 = lookupTime locH1 atm
          rH2 = lookupTime locH2 atm
          s1 = M.lookupLE rH1 c
          lOf (_,Seg _ x _) = x 
          mLoud2 = lOf <$> (s1 >>= flip M.lookupGT c . fst)          
      in case (s1,mLoud2) of
        (Just (x1,Seg x2 loud1 _),Just z)
             -> OneCurve (insertHp x1 x2 rH1 rH2 locH1 locH2 loud1 z c) ts tm
        _    -> throwMine $ printf ("something wrong in hairpin at %s; " ++
                "it needs to be situated entirely between dynamic marks")
                (showLoc2 locH1)


-- <beg/end time of segment into which we are inserting>
-- <beg/end time of hairpin>
-- <beg/end Loc of hairpin>
-- <beg/end loudness>
data HairpinInsert = HairpinInsert (Double,Double) (Double,Double) (Loc,Loc)
                     (Double,Double) (Map Double Seg)
        
{-               
      let s1 = M.lookupLE locH1 c
          mLoud2 = lOf <$> (s1 >>= flip M.lookupGT c . fst)
          lOf (_,(_,(x,_))) = x
      in case (s1,mLoud2) of
           (Just (loc1,(loc2,(loud1,_))),Just z)
             -> insertHp loc1 loc2 locH1 locH2 loud1 z c
           _ -> throwMine $ printf ("something wrong in hairpin at %s; " ++
                "it needs to be situated entirely between dynamic marks")
                (showLoc2 locH1)
-}

insertHp :: Double -> Double -> Double -> Double -> Loc -> Loc ->
            Double -> Double -> Map Double Seg -> Map Double Seg
insertHp segL1 segL2 hpL1 hpL2 hpLoc1 hpLoc2 loud1 loud2 c
  -- in this case, hairpin is exactly coincident with segment: we insert
  -- one new segment
  | segL1 == hpL1 && segL2 == hpL2 =
      M.insert segL1 (Seg segL2 loud1 loud2) c2
  -- in this case, haipin end coincides with segment end; we insert two
  -- segments, a flat one to the left and ramped one to the right
  | segL1 < hpL1 && segL2 == hpL2 =
      M.insert segL1 (Seg hpL1  loud1 loud1) .
      M.insert hpL1  (Seg segL2 loud1 loud2) $ c2
  -- here hairpin beg coincides with segment beg; we still do the old strategy
  -- of assuming hairpin ends right at new dynamic, so we just insert one
  -- segment. Also we assure that hairpin ends within 3 seconds of new
  -- dynamic.
  | segL1 == hpL1 && hpL2 < segL2 && hpL2 >= segL2-3 =
      M.insert segL1 (Seg segL2  loud1 loud2) $ c2
  -- here hairpin starts after the segment and ends before the segment end.
  | segL1 < hpL1 && hpL2 < segL2 && hpL2 >= segL2-3 =
      M.insert segL1 (Seg hpL1  loud1 loud1) .
      M.insert hpL1  (Seg segL2 loud1 loud2) $ c2
  | otherwise = throwMine msg 
  where
    msg = printf ("check hairpin spanning %s -> %s. Hairpin should be " ++
          "contained within dynamic marks and within three seconds of " ++
          "the ending mark") (showLoc2 hpLoc1) (showLoc2 hpLoc2)
    c2 = M.delete segL1 c




{-
    step c (loc1,Hairpin _ loc2) = case M.lookupLE loc1 c of
      Nothing -> throwMine $ printf "no dynamic before hairpin at %s"
                 (showLoc2 loc1)
      Just (locA,(locB,(v1,v2)))
        | locB < loc2 -> throwMine $ printf ("hairpin at %s overshooets " ++
                         "following dynamic") (showLoc2 loc1)
        | loc1 > locA -> let k1 = locA
                             v1 = (loc1,(v1,v1))
                             k2 = loc1
                             v2 = (locB,(v1,v2))
                         in M.insert k1 v1 . M.insert k2 v2 . M.delete locA c
        | loc1 == locA -> M.insert locA (locB,(v1,
-}


{-
doHairpins :: Map Loc Hairpin -> Map Loc [DynData] -> Map Loc p

doHairpins :: Map Loc Hairpin -> OneCurve -> OneCurve
doHairpins hps c = foldr oneHairpin c (M.toList hps)
  where
    oneHairpin :: (Loc,Hairpin) -> OneCurve -> OneCurve
    oneHairpin (loc1,Harpin _ loc2) = M.fromList . concatMap g . M.toList
      where
        g :: (Loc,(Loc,(Double,Double))) -> [(Loc,(Loc,(Double,Double)))]
        g (loc1,loc2,(d1,d2))
              
-}


doDynamics :: TimeSigs -> AbsTimeMap -> Loc -> Map Loc [Dynamic] -> OneCurve
doDynamics timeSigs atm end dyns
  | null z = throwMine "at least one dynamic must be on each staff"
  | otherwise = buildCurve timeSigs atm $ map g z
  where
    l = (M.toAscList $ M.mapWithKey toOneDyn $ dyns) ++ [(end,SimpleDyn 4 1)]
    z = zip l (tail l)
    toOneDyn :: Loc -> [Dynamic] -> Dynamic
    toOneDyn loc ds = case ds of
      [x] -> x
      _   -> throwMine $ printf "at %s, there are 2 or more dynamic marks"
               (showLoc2 loc)
    g :: ((Loc,Dynamic),(Loc,Dynamic)) -> (Loc,(Loc,(Double,Double)))
    g ((loc1,SimpleDyn l _),(loc2,_))
        = (loc1,(loc2,(fromIntegral l,fromIntegral l)))




----------------------------------------------------------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
----------------------------------------------------------------------
--         computing loudness curve expressing patterns


{-

-- makePatternLoudCurve
--
-- A loudness curve is a Map Loc (begin Loc) to segment data DynSeg Loc. Our
-- input here is a map of Loc (begin Loc) to a pair (<dynamics>,<tempo>).
--
makePatternLoudCurve numMeasures (PatternData pats)
  | null segs = throwMine "internal error in Dynamics.hs:patternLoudness"
  | otherwise = M.fromList $ map g segs
  where
    -- We need to add a segment ffrom final 
    locLoudnessPairs :: [(Loc,Double)]
    locLoudnessPairs = map (second fst) (M.toList pats) ++
                       [(Loc (numMeasures+1) 1,0)]
    segs = zip locLoudnessPairs $ drop 1 locLoudnessPairs
    g ((l1,d),(l2,_)) = (l1, DynSeg d d l2)

-}


----------------------------------------------------------------------
----------------------------------------------------------------------


