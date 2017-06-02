{-# LANGUAGE TupleSections #-}
module Util.Map where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad
import Control.Arrow
import Data.Map(Map)
import Data.Maybe
import Data.Monoid
import Util.Exception


mapKeysMaybe :: Ord k1 => (k1 -> Maybe k1) -> Map k1 a -> Map k1 a
mapKeysMaybe f = M.fromList . mapMaybe g . M.toList
  where
    g (key,value) = case f key of
      Nothing -> Nothing
      Just newKey -> Just (newKey,value)


-- justPrevious
--   Given a key K and a map M, look to see what value occurs in M at K,
--   or if K is not in M, then look for the largest key not larger than K.
--   Returns Nothing if no key is <= K.
lookupMaxLower :: Ord k => k -> Map k a -> Maybe a
lookupMaxLower k m = 
    case M.splitLookup k m of
      (_,Just x,_) -> Just x
      (left,Nothing,_) -> fmap fst . M.maxView $ left

-- mapJustPreviousWithKey
--   Given a key K and a map M, look to see what value occurs in M at K,
--   or if K is not in M, then look for the largest key not larger than K.
--   Returns Nothing if no key is <= K.
lookupMaxLowerWithKey :: Ord k => k -> Map k a -> Maybe (k,a)
lookupMaxLowerWithKey k m = 
    case M.splitLookup k m of
      (_,Just x,_) -> Just (k,x)
      (left,Nothing,_) -> fmap fst . M.maxViewWithKey $ left

-- mapListJustPreviousFiltered
lookupListMaxLowerFilt :: Ord k => (a -> Bool) -> k -> Map k [a] -> 
                                Maybe [a]
lookupListMaxLowerFilt pred key
  = lookupMaxLower key . mapListElemsMaybe f
  where
    f a = if pred a then Just a else Nothing

-- mapOneJustPreviousFiltered
lookupOneMaxLowerFilt :: Ord k => (a -> Bool) -> k -> Map k [a] ->
                               Maybe a
lookupOneMaxLowerFilt pred k = 
  join . fmap listToMaybe . lookupListMaxLowerFilt pred k

-- mapJustFollowing
--   Given a key K and a map M, look to see what value occurs in M at K,
--   or if K is not in M, then look for the key just larger than K.
--   Returns Nothing if no key is >= K.
lookupMinUpper :: Ord k => k -> Map k a -> Maybe a
lookupMinUpper k m = 
  case M.splitLookup k m of
    (_,Just x,_) -> Just x
    (_,Nothing,right) -> fmap fst . M.minView $ right

-- mapJustFollowingWithKey
--   Given a key K and a map M, look to see what value occurs in M at K,
--   or if K is not in M, then look for the key just larger than K.
--   Returns Nothing if no key is >= K.
lookupMinUpperWithKey :: Ord k => k -> Map k a -> Maybe (k,a)
lookupMinUpperWithKey k m =
  case M.splitLookup k m of
    (_,Just x,_) -> Just (k,x)
    (_,Nothing,right) -> fmap fst . M.minViewWithKey $ right


lMapMap :: Ord k => (k -> a -> b) -> Map k [a] -> Map k [b]
lMapMap f = M.mapWithKey (\k xs -> map (f k) xs)


mapListElemsMaybe :: Ord k => (a -> Maybe b) -> Map k [a] -> Map k [b]
mapListElemsMaybe = throwMine "replace mapListElemsMaybe with lMistMaybe"


lMapMaybe :: Ord k => (a -> Maybe b) -> Map k [a] -> Map k [b]
lMapMaybe f = M.mapMaybe $ mconcat . map (fmap (:[]) . f)


lMapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> Map k [a] -> 
                        Map k [b]
lMapMaybeWithKey f = listToLMap . mapMaybe g . lMapToList
  where
    g (k,a) = case f k a of {Just b -> Just (k,b); Nothing -> Nothing}


filterListElems :: Ord k => (a -> Bool) -> Map k [a] -> Map k [a]
filterListElems pred = M.mapMaybe (\l -> if null l then Nothing else Just l) .
                       M.map (filter pred)
   

lMapToList :: Ord k => Map k [a] -> [(k,a)]
lMapToList m = concatMap (\(k,xs) -> zip (repeat k) xs) $ M.toAscList m

lMapToDescList :: Ord k => Map k [a] -> [(k,a)]
lMapToDescList m = concatMap (\(k,xs) -> zip (repeat k) xs) $ M.toDescList m

listToLMap2 :: Ord k => [(k,a)] -> Map k [a]
listToLMap2 = M.fromListWith (++) . map (\(x,y) -> (x,[y]))

listToLMap :: Ord k => [(k,a)] -> Map k [a]
listToLMap = M.map reverse . listToLMap2


reverseLMap :: (Ord k, Ord a) => Map k [a] -> Map a [k]
reverseLMap = listToLMap . map (\(x,y) -> (y,x)) . lMapToList


findLMapLT :: Ord k => (a -> Bool) -> k -> Map k [a] -> Maybe (k,a)
findLMapLT pred key m = L.find (pred . snd) . 
                        lMapToDescList . fst . M.split key $ m


findLMapGT :: Ord k => (a -> Bool) -> k -> Map k [a] -> Maybe (k,a)
findLMapGT pred key m = L.find (pred .snd) . lMapToList . snd . M.split key $ m


reduceLMap :: Ord k => (a -> Maybe b) -> Map k [a] -> Map k b
reduceLMap g = M.mapMaybe h 
  where
    h = getFirst . mconcat. map (First . g)
  

mMapRange :: Ord k => (k -> a -> a) -> k -> k -> Map k a -> Map k a
mMapRange g k1 k2 m = M.unions [lower, M.mapWithKey g mid, upper]
  where
    (lower, midUpper) = splitLE k1 m
    (mid  ,    upper) = splitLE k2 midUpper


-- splitInclude
--
-- Split a map at key 'k' and INCLUDE (k,a) in each map (rather than
-- exclude it as done by Data.Map library 'split')
splitInclude :: Ord k => k -> Map k a -> (Map k a, Map k a)
splitInclude k m = case M.splitLookup k m of
  (lower,Nothing,upper) -> (lower,upper)
  (lower,Just x,upper) -> (M.insert k x lower, M.insert k x upper)


splitLE :: Ord k => k -> Map k a -> (Map k a, Map k a)
splitLE k m = case M.splitLookup k m of
  (lower,Nothing,upper) -> (lower,upper)
  (lower,Just x ,upper) -> (lower,M.insert k x upper)


-- lookupBracketing
--
-- Given a key 'kIn', find the greatest key less than or equal to kIn,
-- and the smallest key greater than kIn. 
--
-- For each of these return Just (<key>,<value>) if such a key exists
-- or Nothing if no key fits the criteria.
--
-- This is useful in determining what musical markings are in effect
-- at a given location; for example, pedal markings. Given a note at loc1,
-- you want to find the pedal marking at or just before loc1, and the pedal
-- marking following loc1
lookupBracketing :: Ord k => k -> Map k a -> (Maybe (k,a), Maybe (k,a))
lookupBracketing key1 map1 = (retLow, retHigh)
 where
  (lower,at,upper) = M.splitLookup key1 map1
  retLow = case (at, M.null lower) of
    (Nothing,True) -> Nothing
    (Nothing,False) -> Just $ M.findMax lower
    (Just x,_) -> Just (key1,x)
  retHigh = if M.null upper
            then Nothing
            else Just $ M.findMin upper

-- combineWithPrevious
--   Combines each element with the one occuring just before it.
--   If its the first element, set to Nothing
combineWithPrevious :: Ord k => Map k a -> Map k (a, Maybe a)
combineWithPrevious m = M.fromList combinedWithPrevious
  where
    (smallestKey, smallestVal) = case M.size m of
      0 -> throwMine "zs238"
      _ -> M.findMin m 
    list1 = M.toList m
    -- combinedWithPrevious_ :: [(k,(a, Maybe a))]
    combinedWithPrevious_ = 
      zipWith (\(key2,val2) (_,val1) -> (key2, (val2, Just val1)))
              (tail list1) list1
    combinedWithPrevious = (smallestKey, (smallestVal, Nothing)) : 
                           combinedWithPrevious_

-- combineWithFollowing
--   Combines each element with the one occuring just before it.
--   If its the first element, set to Nothing
combineWithFollowing :: Ord k => Map k a -> Map k (a, Maybe a)
combineWithFollowing m = M.fromList combinedWithFoll
  where
    (largestKey, largestVal) = case M.size m of
      0 -> throwMine "zs238"
      _ -> M.findMax m 
    list1 = M.toList m
    combinedWithFoll_ = 
      zipWith (\(key1,val1) (_,val2) -> (key1, (val1, Just val2)))
              list1 (tail list1)
    combinedWithFoll = combinedWithFoll_++[(largestKey, (largestVal, Nothing))]


{-

mapMapM :: (Monad m, Ord k) => (a -> m b) -> Map k a -> m (Map k b)
mapMapM g mapIn = do
  let h (k,a) = do
        b <- g a
        return (k,b)
  y <- mapM h (M.toList mapIn)
  return $ M.fromAscList y

mapKeysElemsMapM :: (MonadError m, Ord k) => (k -> a -> m (k,b)) 
               -> Map k a -> m (Map k b)
mapKeysElemsMapM g mapIn = do
  let h (k,a) = do
        (k,b) <- g k a
        return (k,b)
      te = throwMine "Error: duplicate keys in mapKeysElemsMapM."
  y <- mapM h (M.toList mapIn)
  return $ M.fromListWith te y
-}


----------------------------------------------------------------------
----------------------------------------------------------------------

flipMap :: (Ord a, Ord b) => Map a (Map b c) -> Map b (Map a c)
flipMap   =
  M.map M.fromList .
  -- 
  M.fromListWith (++) .
  -- [ (b,[a,c]) ]
  map (\(x,(y,z)) -> (y,[(x,z)])) .
  -- [(a,(b,c))] : so these are basically groups of three. Every 'a' that has
  -- a map entry with N pairs will be represented by N of these triples.
  --
  -- If there is a map entry for 'x1' that maps 'y1' to 'z1', then
  -- (x1,(y1,z1)) will appear in this list. And (y1,[(x1,z1)]) will appear in
  -- transformed list. 'y1' may also appear in maps associated with x2 and x3.
  concatMap (\(x,ys) -> map (x,) ys) .
  -- [ (a, [(b,c)] ) ]
  map (second M.toList) .
  -- [(a, Map b c)]
  M.toList 
