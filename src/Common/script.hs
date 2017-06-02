{-# LANGUAGE TupleSections #-}
import Data.List
import Control.Applicative
import Util.Map
import Data.Map(Map)
import Data.Ratio
import Common.CommonExport

a10 = floor $ 4 % 2
a11 = Loc 1 (3%2)

f1 (Loc m b) = Loc m (fromIntegral $ floor b)

d1 = (3,4,5) :: (Int,Int,Int)
d2 = (3,4,6) :: (Int,Int,Int)
d3 = (3,5,7) :: (Int,Int,Int)
d4 = (5,4,5) :: (Int,Int,Int)

t1 = sort [d1,d2,d3,d4]

x = (6+1) `mod` 7

x10 = Just (+3) <*> Just 4
x11 = Nothing <*> Just 4

x12 = (,3)

x13 :: Map Int String
x13 = listToLMap [(3,'a'),(3,'b')]

x14 = lMapToList x13
