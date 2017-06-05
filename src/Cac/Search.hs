{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Cac.Search where

import Data.Map(Map)
import Data.Function
import Data.List(sortBy)
import Control.Monad
import Control.Monad.State
import System.Random

class Opt comp step | comp -> step where
  oSize :: comp -> Int
  oList  :: comp -> [step]
  oApply :: step -> comp -> comp
  oScore :: comp -> Double
  oScores :: comp -> [((Double,Double),Map String Double)]
     -- score at the point it had N


data OptConfig = OptConfig
  { ocTargetSize         :: Int
  , ocMonteCarloDepth    :: Int
  , ocFractionNext       :: Double
  , ocFractionMonteCarlo :: Double
  }

type Test a = a -> Maybe Double

data RioState = RioState
  { rsStdGen :: StdGen
  , rsRecord :: Map String [Integer]
  }


