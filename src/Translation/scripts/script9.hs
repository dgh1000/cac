
import qualified Data.Map as M
import Data.Ratio
import Data.Map(Map)
import Data.Maybe
import Translation.TranslationData
import Common.CommonData
import Common.CommonUtil

x :: Map Int TimeSig -> [Loc]
x timeSigs = xs ++ [Loc (length timeSigs + 1) 1]
  where
    xs = concatMap doMsr [1..length timeSigs]
    doMsr :: Int -> [Loc]
    doMsr n = concatMap doBeat [1..num]
      where
        spb = fromIntegral configSlicesPerBeat
        num = tsNumer . fromJust . M.lookup n $ timeSigs
        doBeat :: Int -> [Loc]
        doBeat b = 
          map 
          (\s -> Loc n (fromIntegral b + s % spb))
          [0.. spb-1]

ts = M.fromList [ (1, TimeSig 1 4)
                , (2, TimeSig 1 4) ]

allSliceLocs :: Map Int TimeSig -> [Loc]
allSliceLocs timeSigs = concatMap doMsr [1..length timeSigs]
  where
    doMsr :: Int -> [Loc]
    doMsr n = concatMap doBeat [1..num]
      where
        spb = fromIntegral configSlicesPerBeat
        num = tsNumer . fromJust . M.lookup n $ timeSigs
        doBeat :: Int -> [Loc]
        doBeat b = 
          map 
          (\s -> Loc n (fromIntegral b + s % spb))
          [0.. spb-1]

main = putStrLn . unlines . map showLoc2 $ allSliceLocs ts

spb = 4 :: Integer

pairToSlices :: Map Int TimeSig -> ((Loc,Rational),(Loc,Rational)) -> 
                [(Loc,Double)]
pairToSlices timeSigs _ = error "foo"

{-

Slices every 2. 


0     3       6           10
0  2     4    6    8      10

for each pair (a,b), generate slices starting >= a and continuing to < b

that means given a, we need to find ceiling to get next slice of >= loc

need function to round to slice

-}


{-

-- Generate list of slice Locs.
-- 
-- Slice locs are all locations which occur within a measure at a multiple of
-- 1%spb beats. (spb=slicesPerBeat)
--
-- The first slice loc it generates will be the smallest one >= l1. This can
-- be computed by calling sliceLocCeil.
--
-- The last loc will be the largest one < l2. Note this is the same as saying
-- the largest one <= (l2 - 1%spb) = l3. I guess we'll use unfoldr to keep
-- adding one slice delta until we get to one > l3.
--
-- Note that we can't generate a slice in this process that is past the final
-- valid location for use in locNormalize (and thusly locAdd). The max
-- possible valid Loc is Loc (N+1) 1 where N is the number of measures in
-- timeSigs. Call this L_max. Let's assume l2 <= L_max and l1 < L_max. In the
-- course of computing potential slice locations, we will cause an error if we
-- try to add something to a Loc that makes it greater than L_max. To
-- guarantee we don't do that:
--
--   because l1 < L_max, ceiling of l1 is <= L_max
--
--   l2 <= L_max. therefore l3 <= L_max - 1%spb. Since we never try to add a
--   slice delta to a number that is > l3, we won't go over L_max.
--
sliceLocRange :: Map Int TimeSig -> Loc -> Loc -> [Loc]
sliceLocRange timeSigs l1 l2 = 
  where
    g x | x > l3    = Nothing
        | otherwise = Just (x, locAdd x (1%spb))
    l1_ceil = sliceLocCeil timeSigs l1
    l3 = locDiff timeSigs l2 (1%spb)


sliceLocFloor :: Loc -> Loc
sliceLocFloor (Loc msr beat) = Loc msr $ floor (beat*(spb%1)) % spb

sliceLocCeil :: Map Int TimeSig -> Loc -> Loc
sliceLocCeil timeSigs (Loc msr beat) = case locNormalize timeSigs l of
    Just x -> x
  where
    l = Loc msr $ ceiling (beat*(spb%1)) % spb


allSlices :: Map Int TimeSig -> [(Loc,Rational)] -> [(Loc,Double)]
allSlices _ _ = error "foo"

timeSigs = M.fromList
  [ (1,TimeSig 4 4)
  , (2,TimeSig 4 4) 
  ]


locx = [ Loc 1 (1  )
       , Loc 1 (4 + 11%12) ]


main = putStrLn . unlines . map (showLoc2 . sliceLocCeil timeSigs) $ locx

-}


