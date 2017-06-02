module Util.CLib.CLibData where

import Data.Map(Map)

-- (Map <time> (Map <freq> <ampl>)) <any associated fundamental freq>
data PvsDump = PvsDump (Map Int (Map Float Float)) Float

data PvsAnalysisChannel = PvsLeftChOnly | PvsRightChOnly | PvsAverageChs
                          deriving(Show)

{-
data PvsAnalysisConfig = PvsAnalysisConfig
  { pacFFTSize :: Int
  , pacOverlap :: Int
  , pacWinSize :: Int
  , pacTableSize :: Int
  , pacChannel :: PvsAnalysisChannel
  , pacSkip :: Int  {- # of frames skipped from beginning of sound file -}
  , pacDur :: Float  {- duration of sound file reading/analyzing -}
  }
-}
