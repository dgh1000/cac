{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception
import Data.Typeable

t1 :: Int -> Int
t1 x = 10 `div` x

t2 = t1 0

t3 :: IO (Either SomeException ())
t3 = try (print (t1 0))

myTry :: IO a -> IO (Either MyException a)
myTry = try

arithTry :: IO a -> IO (Either ArithException a)
arithTry = try

t4 = do
  s <- getLine
  
  s' <- myTry $ arithTry $ process s
  print s'


data MyException = MyException
      deriving (Show, Typeable)

instance Exception MyException
  
{-toException MyException = SomeException
  fromException (SomeException MyException) = Just MyException
  fromException (SomeException _) = Nothing
-}

process :: String -> IO Int
process s = do
  let i = (read s)
  if i == 1
     then throw MyException
     else evaluate $ 10 `div` i

process2 :: String -> IO Int
process2 s = return (read s) -- throw MyException

-- try :: IO a -> IO (Either e a)

testReads = reads "[1,2,3]" :: [([Int],String)]
