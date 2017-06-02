{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import Text.Printf
import Data.Map(Map)
import Data.Ratio
import Instruments.InstrumentsData
import Instruments.ApplyTimeMod
import Score.ScoreData
import Common.CommonData
import Common.CommonUtil


timeSigs = M.fromList [ (1, TimeSig 4 4)
                      , (2, TimeSig 4 4)
                      , (3, TimeSig 4 4)
                      , (4, TimeSig 4 4) ]

testUnit = Unit2Modify (Loc 1 3) (Loc 2 1) (Loc 3 1)
                       (TmRampParab 1 0.5 False)
                       (TmRampParab 2   1  True) (Just 1)
 

testUnit2 = UnitAdjust Nothing (Loc 1 4) (Loc 2 1) (Loc 2 4) (1%4)

mkTm :: Map Loc Double
mkTm = M.fromList . map (,1.0) $ concatMap mkMsrLocs [1..3]
  where
    mkMsrLocs m = map (Loc m) [1,5%4..19%4]


showTm (RelTimeMap t) = concatMap g $ M.toAscList t
  where
    g (l,d) = printf "%s %8.3f\n" (showLoc2 l) d


sumLocs loc1 loc2 (RelTimeMap tm) =
    sum . map snd . filter pred $ M.toAscList tm
  where
    pred (l,_) = loc1 <= l && l < loc2

main = do
  let t = applyTimeMod timeSigs (RelTimeMap mkTm) testUnit2
      s = sumLocs (Loc 1 4) (Loc 2 1) t
  putStrLn $ showTm t
  putStrLn $ printf "%8.3f" s
