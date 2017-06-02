
module Util.List where

import qualified Data.List as L
import Text.Printf
import Common.CommonData
import Common.CommonUtil
import Util.Exception


----------------------------------------------------------------------
----------------------------------------------------------------------
--    pairTags
--
--  Use case: we are applying a tempo modification pattern. We have a list of
--  tempo alterations A_1, A_2, etc. at specific Locs. We need to modify the
--  slices in the time map: S_1, S_2, etc.
--
--  Slow way to do it: we put A_1, A_2, ... into a Map, called the alteration
--  map. Then we take the map of time slices S_1, S_2, and for each time
--  slice, look up the correponding tempo alteration and modify that time
--  slice. That would be an N*Log(N) algorithm.
--
--  Here we try to do it faster.    


pairTags :: (LocKey t, LocKey i) => String -> [t] -> [i] -> [(t,i)]
pairTags errMsg ts is = snd $ L.mapAccumL step (L.tails ts) is
  where
    -- step :: [[t]] -> i -> ([[t]],(t,i))
    step tss i = case searchTag tss i of
      STItemBeforeTag -> throwMine $ printf ("'%s': in List.hs:pairTags," ++
                         " there was an item before the next tag in the " ++
                         "sequence. Consider that either the items or the " ++
                         "tags are not in order. A specific example: if "++
                         "the tags are tempo alterations and the items are "++
                         "time map slices, then there was a time map slice "++
                         "before the next tempo alteration") errMsg
      STNoTagsLeft    -> throwMine $ printf ("'%s': in Lilst.hs:pairTags," ++
                         "internal error (STNoTagsLeft)") errMsg
      STTag xss x     -> (xss,(x,i))


class LocKey x where
  locKey :: x -> Loc


data TagResult t = ItemPastTag
                 | ItemBeforeTag
                 | NoTagsLeft
                 | Tag t
                   deriving(Show)


tagApplies :: (LocKey t, LocKey i) => [t] -> i -> TagResult t
tagApplies [] _ = NoTagsLeft
tagApplies [t] i       | locKey i < locKey t   = ItemBeforeTag
                       | otherwise             = Tag t
tagApplies (t1:t2:_) i | locKey i < locKey t1  = ItemBeforeTag
                       | locKey i >= locKey t2 = ItemPastTag
                       | otherwise             = Tag t1


data SearchTagResult t = STItemBeforeTag
                       | STNoTagsLeft
                       | STTag [[t]] t


searchTag :: (LocKey t, LocKey i) => [[t]] -> i -> SearchTagResult t
searchTag [] _ = STNoTagsLeft
searchTag (ts:tss) i = case tagApplies ts i of
  ItemBeforeTag -> STItemBeforeTag
  NoTagsLeft    -> STNoTagsLeft
  ItemPastTag   -> searchTag tss i
  Tag t         -> STTag (ts:tss) t

----------------------------------------------------------------------
----------------------------------------------------------------------


{-
data AAL = AAL 
applyAttributeList :: ApplyKey a, b => [a] -> [b] -> [(a,Maybe b)]
applyAttributeList
  where
    g :: [[b]] -> a -> ([[b]],(a,Maybe b))
-}





allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs lis = zip (repeat head_) tail_ ++ allPairs tail_
  where
    head_ = head lis
    tail_ = tail lis

-- interleaveAscendingBy
--   Interleave two lists to keep ascending order. they must be in order
--   to start.
interleaveAscendingBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
interleaveAscendingBy _ [] ys = ys
interleaveAscendingBy _ xs [] = xs
interleaveAscendingBy g (x:xs) (y:ys) = case g x y of
  LT -> x : interleaveAscendingBy g xs (y:ys)
  GT -> y : interleaveAscendingBy g (x:xs) ys
  EQ -> x : y : interleaveAscendingBy g xs ys

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (x:remx) (y:remy) 
  | x == y  = x : commonPrefix remx remy
  | otherwise = []

shortestDistinctPrefixes :: Eq a => [a] -> [a] -> Maybe ([a],[a])
shortestDistinctPrefixes list1 list2
  | length list1 <= le || length list2 <= le = Nothing
  | otherwise = Just (take (le+1) list1, take (le+1) list2)
  where 
    cp = commonPrefix list1 list2
    le = length cp

-- choose N elements from a list, and list all those possibilities
chooseN :: [Int] -> Int -> [[Int]]
chooseN list m = chooseN' [] m list
chooseN' :: [Int] -> Int -> [Int] -> [[Int]]
chooseN' listSoFar m []
  | length listSoFar == m = [listSoFar] 
  | otherwise = []
chooseN' listSoFar m (x:xs) 
  | length listSoFar == m = [listSoFar]
  | otherwise = (chooseN' (x:listSoFar) m xs) ++ (chooseN' listSoFar m xs)

takeWhileWithIdx :: (a -> Int -> Bool) -> [a] -> [a]
takeWhileWithIdx predIn listIn = 
  map fst  (takeWhile predN (zip listIn [0..]))
  where
    predN (value,i) = predIn value i

updateListElem :: Int -> a -> [a] -> [a]
updateListElem n x xs 
  | n < 0 || n >= length xs = error "jERT"
  | otherwise = take n xs ++ [x] ++ drop (n+1) xs

alterListElem :: Int -> (a -> a) -> [a] -> [a]
alterListElem n g xs
  | n < 0 || n >= length xs = error "UR678"
  | otherwise = take n xs ++ [g (xs!!n)] ++ drop (n+1) xs
