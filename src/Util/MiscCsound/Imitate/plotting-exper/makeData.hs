{-

- Given an input directory with pvsanal data dumps, create text files
  for gnuplot to read, 

-}

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
import Util.MiscCsound.CLib.GnuPlotInterface

doLog :: Float -> Float
doLog x | x <= 0 = -60
        | otherwise = 20*(logBase 10 x)


-- curveTransformFilterLow
curveTransformFilterLow :: Float -> Map Float Float -> Map Float Float
curveTransformFilterLow cutoff = M.filterWithKey (\k _ -> k >= cutoff)

curveTransformThirdOctaveHist :: Float -> Map Float Float -> Map Float Float
curveTransformThirdOctaveHist center1 c = 
  let c2 = thirdOctaveHist center1 c
  in M.filter (\a -> a /= 0) c2

-- oneDataFile
-- Inputs
--   Map Float Float -> Map Float Float :: transform the curve as needed
--              (i.e. filtering or smoothing)
--   String  :: output directory
--   (Int, Map Float Float) :: (<time in ms>, <freq/amplitude curve)
oneDataFile :: (Map Float Float -> Map Float Float) -> String -> 
               (Int, Map Float Float) -> IO ()
oneDataFile g outdir (t, dat) = do
  let outfilename = printf "%s/g%06d.txt" outdir t
      text = unlines . map onePair $ M.toAscList (g dat)
      -- onePair :: (Float,Float) -> String
      onePair (f,a) = printf "%f %f" f (doLog a)
  B.writeFile outfilename (B.pack $ map (fromIntegral . ord) text)


isTxtFile = (== ".txt") . takeExtension

removeTextFiles dir = do    
  c <- getDirectoryContents dir
  let cf = filter isTxtFile c
  putStrLn $ printf "Removing %d text files.\n" (length cf)
  forM_ cf (\filename ->
    removeFile $ dir ++ "/" ++ filename)
  
-- One must generate the pvsanal data first using something like
-- my file toRunPvsanal.csd (modify it first to set configuration, as
-- documented in it)
--
-- Then this routine will make the data files (extension ".txt") for plotting.
main :: IO ()
main = do
  let pvsanalDir = "/Temp/csound/tables/clarinet-C4"
      plotDir = "/Temp/csound/gnuplot/clarinet-C4"
      cutoffFreq = 50  -- don't include any data points below this frequency
  -- remove existing files from directory plotDir
  removeTextFiles plotDir
  pd <- readPvsDump pvsanalDir
  -- make all the gnu plot data files
  mapM_ (oneDataFile (curveTransformThirdOctaveHist 100) plotDir) (M.toAscList pd) 



  
  