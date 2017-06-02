{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Monad.State
import Util.RandMonad
import System.Random

data RMonState = RMonState StdGen
               deriving(Show)

type RMon = State StdGen

instance RandMonad RMon where
  putGen   = put
  getGen   = get

x :: RMon Double
x = rRandomR (0,1)

main = do
  g <- newStdGen
  print $ runState x g
