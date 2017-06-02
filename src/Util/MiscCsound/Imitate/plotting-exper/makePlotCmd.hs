{-

- Given an input directory with pvsanal data dumps, create text files
  for gnuplot to read, 

-}

import Data.List
import System.Directory
import System.FilePath
import Control.Monad
import Debug.Trace
import Data.Maybe
import Data.Char
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Map(Map)
import Util.MiscCsound.CLib.CLibData
import Util.MiscCsound.CLib.PvsanalInterface
import Util.Exception

getT :: String -> Maybe Int
getT s = 
  let ss = drop 1 (dropExtension s)
  in case reads ss of
       [] -> printf "can't read time of '%s'\n" s `trace` Nothing
       (i,_):_ -> Just i

timesOfPlots :: String -> IO [Int]
timesOfPlots dir = do
  cs <- getDirectoryContents dir
  return $ catMaybes (map getT cs)

closestTime :: [Int] -> Int -> Int
closestTime ts t = minimumBy (\x y -> compare (abs (t-x)) (abs (t-y))) ts

-- spacing:  in samples
someTempPlots :: String -> Int -> Int -> IO String
someTempPlots dir spacing n = do
  ts <-  timesOfPlots dir
  let cdCmd = printf "cd \"%s\"\n" dir
      plotCmd = "plot " ++ intercalate " , " (map onePlot [1..n])
      onePlot n = 
       let t = closestTime ts (n*spacing)
       in printf "'g%06d.txt' with lines" t
  return $ cdCmd ++ plotCmd

-- One must generate the pvsanal data first using something like
-- my file toRunPvsanal.csd (modify it first to set configuration, as
-- documented in it)
--
-- Then this routine will make the data files (extension ".txt") for plotting.
main :: IO ()
main = do
  let plotDir = "/Temp/csound/gnuplot/clarinet-C4"
      timeSpacingOfPlots = 0.010 -- seconds
      numPlots = 5
      gnuPlotCommandFile = "/Temp/csound/gnuplot/cmd.txt"
      sampleRate = 44100 :: Float

  let timeSpacingSamples = round (timeSpacingOfPlots * sampleRate)  :: Int
  -- for experiment, make a gnuplot command file with some plots
  text <- someTempPlots plotDir timeSpacingSamples numPlots
  writeFile gnuPlotCommandFile text

  
  