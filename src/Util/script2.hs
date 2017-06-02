{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Control.Monad.Error
import Util.RandomState

type ErrorRand a = ErrorT String (State RandData) a

runER m s = runState (runErrorT m) s
evalER m s = fst $ runER m s

t1 :: ErrorRand Int
t1 = do
  t2
  r <- lift $ rdRandom
  return r

t2 :: MonadError String m => m Int
t2 = throwError "foo"

main = do
  rd <- freshRandData
  case evalER t1 rd of
    Left s -> putStrLn s
    Right x -> putStrLn $ show x
