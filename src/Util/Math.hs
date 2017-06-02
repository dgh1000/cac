 module Util.Math where

import Text.Printf
import qualified Data.List as L
import Data.Ratio
import Util.UtilData
import Util.Exception
----------------------------------------------------------------------
--                   Algorithms

scale_3_2 :: Fractional a => a -> a -> a -> a -> a -> a
scale_3_2 = scale

scale :: Fractional a => a -> a -> a -> a -> a -> a
scale y1 y2 y3 x1 x3 =
    let f = (y2-y1)/(y3-y1)
    in x1 + f*(x3-x1)

scaleClip :: (Ord a, Fractional a) => a -> a -> a -> a -> a -> a
scaleClip y1 y2 y3 x1 x3 =
    let actY = max y1 (min y2 y3)
    in scale y1 actY y3 x1 x3

{-
-- stretchCurve
--
--      Adjust x-value of start and end points
stretchCurve :: Curve -> Double -> Double -> Curve
stretchCurve (Curve xMin xMax pts) newXMin newXMax =
    Curve newXMin newXMax (map g pts)
        where g (x,y) = (scale xMin x xMax newXMin newXMax, y)
-}



curveLookup2 :: Double -> Curve -> Double
curveLookup2 x (Curve _ _ pts) = out
  where
    (firstX,lastX) = 
      if length pts <= 1
        then throwMine "ljdks;243"
        else (fst . head $ pts, fst . last $ pts)
    out = if x <= firstX 
      then snd . head $ pts
      else 
        if x >= lastX
        then snd . last $ pts
        else out2
        where
          zipped = zip pts (tail pts)
          out2 = case L.find (\((x1,_),(x2,_)) -> x1 <= x && x <= x2) zipped of
            Just ((x1,y1),(x2,y2)) -> scale x1 x x2 y1 y2
            Nothing -> throwMine $ printf "foo5708 %s %f" (show zipped) x
curveLookup2 _ (CurveConstant x) = x


{-
curveLookup x (Curve _ _ pts) = surrounding pts
    where
      surrounding :: [(Double,Double)] -> Double
      surrounding ((x1,y1) : (x2,y2) : remain)
          | x1 <= x && x <= x2     = scale x1 x x2 y1 y2
          | otherwise = surrounding ((x2,y2) : remain)
      surrounding _ = error "failed in curveLookup"
-}


rationalFloor :: Rational -> Integer
rationalFloor r = n `div` d
  where
    n = numerator r
    d = denominator r

-- intersect
--
-- Return True if the intervals (x1,x2) (y1,y2) intersect.
-- "Just touching" is not considered intersection.
-- Required precondition is that x1 < x2 and y1 < y2. Precondition is not
-- checked.
intersect :: Ord a => a -> a -> a -> a -> Bool
intersect x1 x2 y1 y2 = not (y2 <= x1 || x2 <= y1)


showRationalNicely :: Rational -> String
showRationalNicely r = 
  if remain == 0
    then show i
    else show i ++ " + " ++ show remain ++ "/" ++ show d
  where 
    n = numerator r
    d = denominator r
    i = n `div` d
    remain = n `mod` d

----------------------------------------------------------------------
-- midiPitchToFreq :: (Integral a, Floating b) => a -> b
-- midiPitchToFreq p = 440 * 2 ** ((fromIntegral p - 69)/12)

midiPitchToFreq :: (Integral a, Floating b) => a -> b
midiPitchToFreq p =
  let pCenter = p - 60
      pOct = pCenter `div` 12
      pDegree = pCenter `mod` 12
  in 261.6 * 2**(fromIntegral pOct) * (tuningTable !! (fromIntegral pDegree))

centsToRatio :: Floating b => b -> b
centsToRatio cents = 2 ** (cents/1200)

tuningTable :: Floating b => [b]
tuningTable = 
 [ centsToRatio 0
 , centsToRatio 90
 , centsToRatio 192
 , centsToRatio 294
 , centsToRatio 390
 , centsToRatio 498
 , centsToRatio 588
 , centsToRatio 696
 , centsToRatio 792
 , centsToRatio 888
 , centsToRatio 996
 , centsToRatio 1092 ]

----------------------------------------------------------------------


roundUpPowerOfTwo :: Int -> Int
roundUpPowerOfTwo x = (2^) . ceiling . logBase 2 . fromIntegral $ x

roundToEven :: Double -> Int
roundToEven f = 2 * round (f/2)



toDecibels :: Double -> Double
toDecibels = (* 20) . logBase 10

fromDecibels :: Double -> Double
fromDecibels x = 10 ** (x/20)

clip :: Ord a => a -> a -> a -> a
clip x tmin tmax = min (max tmin x) tmax

computeMSD :: Floating a => [a] -> (a,a)
computeMSD xs = (mean, sqrt (variance / fromIntegral (length xs)))
  where mean = sum xs / (fromIntegral $ length xs)
        variance = sum $ map (\x -> (x-mean)*(x-mean)) xs


-- Given a list of value, change each one to a representation of the
-- number of standard deviations away from the mean.
toSD :: [Double] -> [Double]
toSD xs = map g xs
  where
    (avg,std) = computeMSD xs
    g x = scale (avg-std) x (avg+std) (-1) 1

-- Convert a list of values into a "percentile" form where very lowest value
-- gets assigned 0, highest is 1, and median is 0.5. When there are duplicate
-- values, will use an average method: the computed percentile will be
-- halfway between what it would be using the highest rank and be using the
-- lowest rank.
toPercentile :: [Double] -> [Double]
toPercentile inp = map doValue inp
  where
    sorted = L.sort inp 
    len = length inp
    perc idx = fromIntegral idx / (fromIntegral len - 1)
    doValue x = case L.elemIndices x sorted of
      [i] -> perc i
      is -> ((perc (head is)) + (perc (last is)))/2

