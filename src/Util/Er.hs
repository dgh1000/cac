{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module Util.Er where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import System.Random
import Util.Exception
import Util.RandMonad



doEr m seed = runState (runWriterT (runErrorT (runEr m)))
  (ErState $ mkStdGen seed)

outputEr m seed = fst . fst . doEr m $ seed

-- doEr m seed = fst $ runEr m seed

data ErState = ErState StdGen

newtype Er a = Er { runEr :: ErrorT String (WriterT String (State ErState)) a }
    deriving(Monad, MonadError String, MonadWriter String, MonadState ErState)

instance RandMonad Er where
  putGen g = Er $ put (ErState g)
  getGen = Er $ do {ErState g <- get; return g}

