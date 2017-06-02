{-

Handles interfacing with data produced by pvsanal files. 

Currently

   - code for reading the dumps of tables with pvsanal data

-}
module Util.CLib.PvsanalInterface( readPvsDump
                                 , pvsAnalysis 
                                 , computePvsanalArgsStyle1
                                 , timeRangeToPartialStrengths
                                 , facToPartialStrengths )  where

import Debug.Trace
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import System
import System.IO
import Text.Printf
import System.Directory
import qualified Data.ByteString as B
import Data.Char
import Data.Map(Map)
import qualified Data.Map as M
import Util.CLib.CLibData( PvsDump(..)
                         , PvsAnalysisChannel(..) )
import qualified Util.CLib.CLibData as CLD
import Data.List(sort)
import Util.Math
import Util.Exception
import Util.CLib.DirectoryStuff( removeTextFiles )


readByteString :: Read a => B.ByteString -> a
readByteString = read . map (chr . fromIntegral) . B.unpack

readTableValues :: String -> IO [Float]
readTableValues s = do
  -- okay we read file strictly
  b <- B.readFile s
  -- split into lines. not sure why I needed this
  let bs = B.split (fromIntegral . ord $ '\n') b
      lenLine = bs !! 1
      -- oh we are reading a number from it
      n = readByteString (B.drop 6 lenLine) :: Int
      valueLines = take n . drop 25 $ bs
  return $ map readByteString valueLines

oneFilePair :: String -> (String,String) -> IO (Int,Map Float Float)
oneFilePair dir (ffile,afile) = do
  fvalues <- readTableValues (dir ++ "/" ++ ffile)
  avalues <- readTableValues (dir ++ "/" ++ afile)
  let t = read . take 6 . drop 1 $ ffile
  return (t, M.fromList $ zip fvalues avalues)

-- readPvsFiles
--
-- Input
--   [(String,String)] :: list of file name pair. Each filename pair
--                        has pvsanal output:
--                       ( <file with time data>, <file with ampl data> )
--   String :: directory of above files
readPvsFiles :: [(String,String)] -> String -> IO (Map Int (Map Float Float))
readPvsFiles filenames dir = do
  -- contents :: [(Int,Map Float Float)]
  contents <- mapM (oneFilePair dir) filenames
  return $ M.fromList contents

----------------------------------------------------------------------
----------------------------------------------------------------------
-- readPvsDump useful for reading pvs data after creating it with csound
----------------------------------------------------------------------
----------------------------------------------------------------------

-- readPvsDump
-- Inputs
--  String :: directory name where pvs dump is
readPvsDump :: String -> IO PvsDump
readPvsDump pvsDumpDir = do
  c <- getDirectoryContents pvsDumpDir
  let afiles = filter ((== 'a') . head) c
      ffiles = filter ((== 'f') . head) c
      filenames = zip (sort ffiles) (sort afiles)
  -- read files into Map Int (Map Float Float)
  -- fileData :: PvsDump
  d <-  readPvsFiles filenames pvsDumpDir
  -- bin is read strictly
  bin <- B.readFile (pvsDumpDir ++ "/fundamental.bin")
  let binLazy = BL.pack . B.unpack $ bin
  return $ PvsDump d (decode binLazy)


-- createPvsanalCsd 
-- 
-- Inputs
--   String :: filename of csd we are computing
--   String :: directory where to place output files when csd runs
--   Int :: fft size
--   Int :: overlap
--   Int :: window size
--   Int :: table size
--   String :: name of sound file to analyze
--   String :: command to select mono or stereo (see the template file)
--   Int :: number of frames to skip in the sound file
--   Float  :: duration of analysis
createPvsanalCsd :: String -> String -> Int -> Int -> Int -> Int -> String ->
                    String -> Int -> Float -> IO ()
createPvsanalCsd csdFileName outDir fftsize overlap winsize tablesize fileName
  monoStereo skip dur = do
  template <- readFile 
    "/Mike/Music/algo/haskell/Util/CLib/runPvsanalTemplate.csd"
  let csd = printf template fftsize overlap winsize tablesize fileName 
            outDir skip monoStereo dur 
  -- let csd = printf template fftsize
  writeFile csdFileName csd

getYOrN :: IO Bool
getYOrN = do
  putStr "Please enter y or n ---> "
  hFlush stdout
  s <- getLine
  if s == "y"
    then return True
    else if s == "n" 
           then return False 
           else getYOrN




----------------------------------------------------------------------
----------------------------------------------------------------------
-- Function 'pvsAnalysis' does pvs analysis with querying the user for
-- confirmation to proceed with erasing all text files from the output
-- directory.
--
-- The old 'pvsAnalysis' function is now broken
----------------------------------------------------------------------
----------------------------------------------------------------------

--pvsAnalysis
--    Runs pvsanal in csound. Also, first removes all text files from
--    the destination directory.

-- Input
--  String :: sound file name (whole path)
--  String :: directory where to put pvs tables
-- Output
--   returns (<success status>, <message>)
pvsAnalysis :: String -> String -> Int -> Int -> Int -> Int -> Maybe Float ->
               PvsAnalysisChannel -> Int -> Float -> IO (Bool,String)
pvsAnalysis soundFileName pvsDataDir fftsize overlap winsize tablesize fund
  channel skip dur = do
  let csdFileName = "generated-pvs.csd"
      channelString = case channel of 
        PvsLeftChOnly -> "asig = asig1"
        PvsRightChOnly -> "asig = asig2"
        PvsAverageChs -> "asig = (asig1+asig2)/2"
  putStrLn $ printf ("About to start pvs analysis.\nShould I remove text " ++
                     "files from'%s'?") pvsDataDir
  flag <- getYOrN
  if flag
    then (do removeTextFiles pvsDataDir
             putStrLn "Running a csd to do pvs analysis...."
             createPvsanalCsd csdFileName pvsDataDir fftsize overlap 
               winsize tablesize soundFileName channelString skip dur
             system $ "csound " ++ csdFileName
             case fund of
               Nothing -> return ()
               Just f -> BL.writeFile (pvsDataDir ++ "/fundamental.bin")
                         (encode f)
             return (True, printf (
                     "\n\nFinished run of pvsanal; tables have been" ++
                     " written to disk.\n\nThe analysis rate was %f ms")
                     (1000*fromIntegral overlap/44100::Float)))
    else return (False, "\n\nUser aborted pvs analysis (didn't confirm removing"++ " tables).")


computePvsanalArgsStyle1 :: Float -> Float -> (Int,Int,Int,Int)
computePvsanalArgsStyle1 sr soundFileFundmental =
  (fftsize,overlap,winsize,tablesize)
  where
    fftsize = roundToEven $ 2*sr/soundFileFundmental
    overlap = ceiling $ fromIntegral fftsize / (4 :: Float)
    winsize = fftsize
    tablesize = roundUpPowerOfTwo fftsize



{-
pvsAnalReportTemplate = unlines
  [ "\n\n\nDo you wish to do the pvs analysis? "
  , "\nGIVEN PARAMETERS: "
  , "  -- Sound file: '%s'"
  , "  -- Pvsanal output dir (text tables): %s"
  , "     (all text files in above dir will be removed first)"
  , "  -- pitch fundamental frequency of: %f"
  , "\n\nCOMPUTED PARAMETERS:"
  , "  -- iffsize:  %7d  (chosen in order to have two bins per partial)"
  , "  -- overlap:  %7d  (choosn as fftsize/4 for no good reason other "
  , "     they said so)" 
  , "  -- winsize:  %7d  (just made it equal to fftsize, don't have "
  , "     any better idea"
  , "  -- tablesize %7d (rounded up the fftsize to next power of two)"
  , " "
  , "The duration of each analysis frame will be %.2f milliseconds"
  , ""
  , "The first %f seconds will be analyzed."
  , ""
  , " -> if you agree, .txt files in the output dir will first be deleted <-" ]
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
-- some routines useful for finding harmonic partial strenths
--   (including averaging over a time range)
----------------------------------------------------------------------
----------------------------------------------------------------------


-- facToPartialStrengths
--   Given a FAC (freq/ampl curve), a fundamental freq and a max freq,
--   make a list of partial strengths ("partial strengths set" or PSS)
facToPartialStrengths :: Float -> Float -> Float -> Map Float Float -> 
                         Map Int Float
facToPartialStrengths fund maxFreq partialWidth fac =
  M.fromList . map (onePartial fac) $ [1..maxPartial]
  where
    maxPartial = round (maxFreq/fund)
    -- onePartial computes the sum of amplitudes within frequency
    --  range (p - 0.5*fund, p + 0.5*fund) where p is the partial frequency
    onePartial :: Map Float Float -> Int -> (Int,Float)
    onePartial freqAmplCurve n = (n,s)
      where
      s = sum . M.elems . M.filterWithKey (\k _ -> low < k && k < high) $ fac
      low = fund * (fromIntegral n - partialWidth/2)
      high = fund * (fromIntegral n + partialWidth/2)
  
-- timeRangeToPartialStrengths
--   Given a PvsDump (which is a set of time-tagged FACs), find all FACs within
--   the given time range, convert each to a partial-strengths set (PSS)
--    and make a single PSS by taking unionsWith (+) and dividing by
--    number of curves. (so we get the average)
-- Inputs
--   PvsDump
--   Float :: fundamental freq
--   Float :: max frequency of a partial
--   Float :: sampling rate
--   Float :: partial width: given a partial N at F*N Hz, we use
--               the specturm power of bins with freqs (F*(N-d/2), F*(N+d/2))
--               where this argument is d
--   Float :: t1 (lower time)
--   Float :: t2 (higher time)
-- Outputs
--   Map Int Float :: (freq/ampl curve) (first partial is 1)
timeRangeToPartialStrengths :: PvsDump -> Float -> Float -> Float -> Float ->
                               Float -> Map Int Float
timeRangeToPartialStrengths (PvsDump pd fund) maxFreq sampRate partialWidth 
  t1 t2 = 
  M.map (/ divisor) u
  where
    t1Samps = round $ t1*sampRate
    t2Samps = round $ t2*sampRate
    facs = M.elems . M.filterWithKey 
           (\time _ -> t1Samps <= time && time <= t2Samps) $ pd
    psss = let facs' = printf "\n%d curves averaged at %f %f" (length facs) t1 t2   `trace` facs
           in map (facToPartialStrengths fund maxFreq partialWidth) facs'
    divisor = if length facs == 0
              then throwMine $ printf
                   ("No freq/ampl curves (FACs) were present in time " ++
                    "range %.2f ms to %.2f ms.") (1000*t1) (1000*t2)
              else fromIntegral $ length facs
    u = M.unionsWith (+) psss
