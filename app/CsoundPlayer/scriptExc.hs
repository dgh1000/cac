{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (catch)
import Control.Exception
import Data.Typeable

data MyException = ThisException | ThatException
                 deriving(Show,Typeable)

instance Exception MyException

handlerIO :: IOException -> IO ()
handlerIO e = putStrLn $ "Caught IO " ++ show e

handlerMine :: MyException -> IO ()
handlerMine e = putStrLn $ "Caught mine " ++ show e

handlerAll :: SomeException -> IO () 
handlerAll e = putStrLn $ "Caught SomeException " ++ show e

mine1 = error "foo"

mine2 = do 
  s <- readFile "foo.txt"
  putStrLn s

mine3 :: IO String
mine3 = do
  putStr "---> "
  s <- getLine

loop :: Int -> IO ()
loop x = do
  putStr "---> "
  s <- getLine
  

main = do
  mine1 `catches` [Handler handlerIO, Handler handlerMine]

