{-# LANGUAGE DeriveDataTypeable #-}

module Util.Exception where

import Control.Exception
import Data.Typeable
import Data.Array as A

data MyException = MyException String
                   deriving (Show, Typeable)

instance Exception MyException

throwMine = throw . MyException

myFromJust :: String -> Maybe a -> a
myFromJust _ (Just x) = x
myFromJust s Nothing = throwMine s

listLookup :: String -> [a] -> Int -> a
listLookup s xs i 
  | i >= length xs || i < 0 = throwMine s
  | otherwise = xs !! i

arrLookup :: (Ix i) => String -> Array i e -> i -> e
arrLookup s a i 
 | i < lo || i > hi = throwMine s
 | otherwise = a A.! i
 where
  (lo,hi) = A.bounds a