module Util.MiscCsound.CLib.GnuPlotInterface {- ( thirdOctaveHist ) -} where

import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M

{-
-- mkCommandSeveralPlotWithLines
-- Inputs
--   String   <directory name>
--   [String] filenames
mkCommandSeveralPlotWithLines :: String -> [String] -> String
mkCommandSeveralPlotWithLines dir fnames =
  printf "cd %s\nplot %s\n" dir (intercalate "," $ map one fnames)
  one s  = print "'%s' with lines"

-}

sumInRange :: Map Float Float -> (Float,Float) -> (Float,Float)
sumInRange curve (low,high) = 
  ( sqrt (low*high)
  , sum . M.elems . M.filterWithKey (\k _ -> k >= low && k < high) $ curve)

mkRanges :: Float -> Float -> [(Float,Float)]
mkRanges center1 maxCenter = 
  map g . takeWhile (<= maxCenter) $ iterate (* ratio) center1
  where 
    ratio = 2
    ratio6 = 2**(1/2)
    g :: Float -> (Float,Float)
    g f = (f/ratio6, f*ratio6)

-- thirdOctaveHist
--
-- Given an input which is curve with key=freq and value=ampl,
-- make a histogram type curve by summing amplitudes of frequencies
-- within a 1/3 octave boundary
-- Inputs
--  Float :: center freq of first 1/3 octave group
thirdOctaveHist :: Float -> Map Float Float -> Map Float Float
thirdOctaveHist center1 curveIn = 
   let maxCenterFreq = 15000
       ranges = mkRanges center1 maxCenterFreq 
   in M.fromList . map (sumInRange curveIn) $ ranges

partialCurveToData :: Map Int Float -> String
partialCurveToData m = unlines . map g . M.toList $ m
  where g (n,v) = printf "%d %f" n v
