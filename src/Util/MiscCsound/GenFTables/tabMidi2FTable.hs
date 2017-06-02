import Data.List
import Control.Arrow
-- index 36 is 36-- 37, 38 both 36, 39, 40 are 40
-- 0 through 38: 36
-- 39 through 127: 40


-- c:     0
-- c#/Db: 1
-- d:     2 
-- d#/Eb: 3
-- E:     4
-- F:     5
-- F#/Gb  6
-- G:     7
-- G#/Ab  8
-- A:     9
-- A#/Bb  10
-- B:     11

--
-- c4,  f#4, c5, f5, c6, g6, d7, f7, Ab7
-- 36,  42,  48, 53, 60, 67, 74, 77, 80
-- 
keysToUse = [36, 42, 48, 53, 60, 67, 74, 77, 80]

-- lis = (take 39 $ repeat 36) ++ (take (128-39) $ repeat 40)

closestUsable x = snd $ minimumBy (\(d1,_) (d2,_) -> compare d1 d2) tups
   where tups = map (\key -> (abs (key-x),key)) keysToUse

-- need to make tables giving right table # to use, and base frequency

makeClosestUsableTable = concatMap (\x -> " " ++ show x) nums
    where nums = map closestUsable [0..127]



freq x = 440 * (2**((x-69)/12))


-- lis2 = (take 39 $ repeat (freq 36)) ++ (take (128-39) $ repeat (freq 40))

{-
main = do
  let nums = concatMap (\n -> " " ++ show n) lis2
  writeFile "out.txt" nums
-}

main = writeFile "out.txt" makeClosestUsableTable
