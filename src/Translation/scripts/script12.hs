
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Common.CommonData
import Common.CommonUtil

ts = M.fromList
  [ (1, TimeSig 4 4)
  , (2, TimeSig 4 4)
  , (3, TimeSig 4 4)
  ]

spb = 8 :: Integer

sliceD = 1%spb

deltaQToTime :: TimeSigs -> RelTimeMap -> Loc -> Rational -> Double
deltaQToTime ts (RelTimeMap tm) locIn deltaQ =
  deltaQ*sliceTime/fromRational sliceQuar
  where
    (loc1,sliceTime) = case M.lookupLE locIn tm of {Just x -> x}
    loc2 = case M.lookupGT locIn tm of {Just (x,_) -> x}
    sliceQuar = locDiffQuars ts loc1 loc2


findPauseLoc :: Map Loc Double -> Loc -> Loc
findPauseLoc m loc = case M.toDescList . fst . M.split locFloor $ m of
    (locP,_):_ -> locP
  where
    locFloor = case M.lookupLE loc m of {Just (x,_) -> x}


makeRtm = M.fromList $ map (\loc -> (loc,1)) ls
  where
    ls = concatMap f [1..3]
    f msrNum = map g [0..spb*4-1]
      where
        g n = Loc msrNum (1+fromIntegral n*sliceD)


showR :: Rational -> String
showR r = printf "%d + %s" i (show j)
  where
    i = floor r :: Int
    j = r-fromIntegral i
 


main1 = putStrLn . showLoc2 $ findPauseLoc makeRtm l1
  where
    l1 = fromJust $ locSub ts (Loc 2 1) (0)
