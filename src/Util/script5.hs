
import Data.Maybe
import Data.Bits

type PCSet = Int

pitchListToPCSet :: [Int] -> PCSet
pitchListToPCSet pitches = foldr g 0 pitches
  where
    -- g :: <pitch> -> current vec
    g :: Int -> Int -> Int
    g pitch currVec = currVec .|. bit (pitch `mod` 12)

pcSetToPitchList :: PCSet -> [Int]
pcSetToPitchList pcset = catMaybes . map g $ [0..11]
  where
    g i | testBit pcset i = Just i
        | otherwise = Nothing

t1 = pitchListToPCSet [13,14, 16]

t2 = pcSetToPitchList t1

t3 = scanl (+) 0 [2, 4, 3, 7]

