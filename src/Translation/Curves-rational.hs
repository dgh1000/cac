
-- curves need idea they may control just about anything or be references for
-- any quantity including computations on other note parameters. will be
-- double mapped to double, no problem always doing that. may be undefined,
-- lookup needs to show when defined or not. when checking if in range, round
-- to 0.001

module Translation.Curves where

import qualified Data.Set as S
import qualified Data.Map as M
import Text.Printf
import Data.Map(Map)
import Data.Set(Set)
import Data.Maybe
import Data.Ratio
import Common.CommonData
import Common.CommonUtil
import Translation.TranslationData
import Translation.TimeMap
import Util.Math
import Util.Map
import Util.Showable

{-

showCurve :: OneCurve -> String
showCurve = unlines . map g . M.toAscList
  where
    g (loc1,(loc2,(d1,d2))) = printf "%s %s %.2f %.2f" (showLoc2 loc1)
                              (showLoc2 loc2) d1 d2


curvesLookup :: AbsTimeMap -> Loc -> [OneCurve] -> Double
curvesLookup atm loc = sum . map (curveLookup atm loc)


curveLookup :: AbsTimeMap -> Loc -> OneCurve -> Double
curveLookup atm loc curve =
  case M.maxViewWithKey . fst . splitInclude loc $ curve of
    Nothing -> 0
    Just ((loc1,(loc2,(d1,d2))),_) | loc <= loc2 -> scale tBeg t tEnd d1 d2
                                   | otherwise   -> 0
      where
        tBeg = lookupTime loc1 atm
        tEnd = lookupTime loc2 atm
        t    = lookupTime loc  atm
  


-- Map Loc (Loc,(Double,Double))

-- how do we find segments that are in range? say (5,10) then we look for
-- anything that starts above 5 but less than 10. Just do GT search on 5 and
-- see if starts less than 10. should curves be gradual by Loc or by time? by
-- time.

--   |     |
--       |     |
--         |       |
--
--         high involves any segment that starts
--
--    | 5      5|
--
--        |1        3|
--              |2       |2
--        (2+5) |   starts before, overlaps

merge :: TimeSigs -> [OneCurve] -> OneCurve
merge timeSigs curves = M.fromList $ map doLocPair locPairs
  where
    m :: (Loc,(Loc,(Double,Double))) -> [Loc]
    m x = [fst x, fst $ snd x]
    g :: OneCurve -> Set Loc
    g = S.fromList . concatMap m . M.toList
    h = S.unions $ map g curves
    locPairs = let x = S.toAscList h in zip x (tail x)
    doLocPair (l1,l2) = let v1 = sum $ map (jVal False l1) curves
                            v2 = sum $ map (jVal True  l2) curves
                        in (l1,(l2,(v1,v2)))
    -- Bool = False means looking up low side value, = True means high side
    -- value. any segment that starts before or equal can potentially be
    -- relevant
    jVal :: Bool -> Loc -> OneCurve -> Double
    jVal flag loc c = case M.lookupLE loc c of
        Nothing -> 0
        Just (locA,(locB,(v1,v2)))
          | overlaps (locA,locB) -> sc locA loc locB v1 v2
          | touchesOnLoSide (locA,locB) && not flag -> v1
          | touchesOnHiSide (locA,locB) && flag     -> v2
          | otherwise -> 0
        
      where
        overlaps (loc1,loc2) = loc1 < loc && loc2 > loc
        touchesOnLoSide (loc1,_) = loc1 == loc
        touchesOnHiSide (_,loc2) = loc2 == loc
        sc loc1 loc2 loc3 x1 x2 = out
          where
            db3 = locDiffQuar timeSigs loc1 loc3
            db2 = locDiffQuar timeSigs loc1 loc2
            out = scale 0 (fromRational db2) (fromRational db3) x1 x2
          
----------------------------------------------------------------------




toCurveDbl :: AbsTimeMap -> OneCurve -> OneCurveDbl
toCurveDbl atm = M.fromList . map doSeg . M.toList
  where
    doSeg (loc1,(loc2,(d1,d2))) = (t1,(t2,(d1,d2)))
      where
        t1 = lookupTime loc1 atm
        t2 = lookupTime loc2 atm


curveDblLookup :: AbsTimeMap -> Double -> OneCurveDbl -> Double
curveDblLookup atm t curve =
  case M.maxViewWithKey . fst . splitInclude t $ curve of
    Nothing -> 0
    Just ((t1,(t2,(d1,d2))),_) | t <= t2   -> scale t1 t t2 d1 d2
                               | otherwise -> 0
  

curvesDblLookup :: AbsTimeMap -> Double -> [OneCurveDbl] -> Double
curvesDblLookup atm t = sum . map (curveDblLookup atm t)


----------------------------------------------------------------------
----------------------------------------------------------------------

-}

buildCurve :: AbsTimeMap -> [(Loc,(Loc,(Double,Double)))] -> OneCurve
buildCurve atm inp = OneCurve (M.fromList segs) (concat pairsOut) 
  where
    (pairsOut,segs) = unzip $ map f inp
    g x = approxRational x configApproxRational
    f :: (Loc,(Loc,(Double,Double))) -> ([(Rational,Loc)],(Rational,Seg))
    f (x1,(x2,(y1,y2))) = ( [(xt1,x1), (xt2,x2)]
                          , (xt1, Seg xt2 (g y1) (g y2))
                          )
      where
        xt1 = g $ lookupTime x1 atm
        xt2 = g $ lookupTime x2 atm


showCurve :: Curve -> ShowItem
showCurve (Curve cs) = Component "Curve" True (map showC cs)
  where
    showC (OneCurve m _) = Component "OneCurve" True
      (map showPair $ M.toAscList m)
    showPair (x1,Seg x2 y1 y2) =
      SingleLine $ printf "(%8.3f,%8.3f) (%8.3f,%8.3f)"
                   (fromRational x1 :: Double) (fromRational x2 :: Double)
                   (fromRational y1 :: Double) (fromRational y2 :: Double)
                   

curveLookupD :: Double -> Curve -> Maybe Double
curveLookupD d c = fromRational $
  curveLookup (approxRational d configApproxRational) c


curveLookup :: Rational -> Curve -> Maybe Rational
curveLookup t (Curve oneCurves) =
  case mapMaybe (oneCurveLookup t) oneCurves of
    [] -> Nothing
    xs -> Just $ sum xs


-- what don't we want to happen? like two same Locs get different answer.
-- Loc doesn't even make it onto the curve
--
-- a number that is exactly on the nose, should it be the prior seg or the 
oneCurveLookup :: Rational -> OneCurve -> Maybe Rational
oneCurveLookup t (OneCurve m _) = case M.lookupLE t m of
  Nothing -> Nothing
  Just (t1,Seg t2 v1 v2)
    | t <= t2   -> Just $ scale_3_2 t1 t t2 v1 v2
    | otherwise -> Nothing
