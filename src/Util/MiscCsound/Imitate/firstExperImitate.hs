{-

A program to run everything needed to analyze a tone for imitative
synthesis.


-}

import Debug.Trace
import Data.Map(Map)
import qualified Data.Map as M
import System
import Text.Printf
import Util.MiscCsound.CLib.DirectoryStuff( removeTextFiles )
import Util.MiscCsound.CLib.PvsanalInterface( createPvsanalCsd
                                            , readPvsDump )
import Util.Math( roundUpPowerOfTwo )
import Util.Exception
import Util.MiscCsound.CLib.CLibData(PvsDump(..))
data Basis = NumBins
           | AnalysisRate

getYOrN :: IO Bool
getYOrN = do
  putStrLn "Please enter y or n:"
  s <- getLine
  if s == "y"
    then return True
    else if s == "n" 
           then return False 
           else getYOrN

computePvsanalParams :: Basis -> Float -> Float -> IO ()
computePvsanalParams bas fund per = do
  case bas of
    NumBins -> do
      putStrLn "I will do the analysis on the basis"

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


roundToEven :: Float -> Int
roundToEven f = 2 * round (f/2)

pvsAnalysis :: String -> String -> Float -> Float -> Float -> IO ()
pvsAnalysis soundFile pvsDataDir sr soundFileFundmental dur = do
  let fftsize = roundToEven $ 2*sr/soundFileFundmental  :: Int
      overlap = ceiling $ fromIntegral fftsize / (4 :: Float)
      winsize = fftsize
      tablesize = roundUpPowerOfTwo fftsize
      frameDurMilliseconds = 1000 * (fromIntegral overlap) / sr
      csdFileName = "generated-dont-edit.csd"
  putStrLn $ printf pvsAnalReportTemplate
             soundFile
             pvsDataDir
             soundFileFundmental
             fftsize
             overlap
             winsize
             tablesize
             frameDurMilliseconds
             dur
  flag <- getYOrN
  if flag 
    then (do removeTextFiles pvsDataDir
             putStrLn "Running a csd to do pvs analysis...."
             createPvsanalCsd csdFileName pvsDataDir fftsize overlap 
               winsize tablesize soundFile "asig = asig1" dur
             system "csound generated-dont-edit.csd"
             -- putStrLn "Skipping csound run for debugging reasons"
             return () )
    else putStrLn "Okay, skipping..."

-- facToPartialStrengths
--   Given a FAC (freq/ampl curve), a fundamental freq and a max freq,
--   make a list of partial strengths ("partial strengths set" or PSS)
facToPartialStrengths :: Float -> Float -> Map Float Float -> Map Int Float
facToPartialStrengths fund maxFreq fac =
  M.fromList . map (onePartial fac) $ [1..maxPartial]
  where
    maxPartial = let m = round (maxFreq/fund) in show m `trace` m
    -- onePartial computes the sum of amplitudes within frequency
    --  range (p - 0.5*fund, p + 0.5*fund) where p is the partial frequency
    onePartial :: Map Float Float -> Int -> (Int,Float)
    onePartial freqAmplCurve n = (n,s)
      where
      s = sum . M.elems . M.filterWithKey (\k _ -> low < k && k < high) $ fac
      low = fund * (fromIntegral n - 0.5)
      high = fund * (fromIntegral n + 0.5)
  
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
--   Float :: t1 (lower time)
--   Float :: t2 (higher time)
-- Outputs
--   Map Int Float :: (freq/ampl curve) (first partial is 1)
timeRangeToPartialStrengths :: PvsDump -> Float -> Float -> Float -> Float ->
                               Float -> Map Int Float
timeRangeToPartialStrengths pd fund maxFreq sampRate t1 t2 = 
  M.map (/ divisor) u
  where
    t1Samps = round $ t1*sampRate
    t2Samps = round $ t2*sampRate
    facs = M.elems . M.filterWithKey 
           (\time _ -> t1Samps <= time && time <= t2Samps) $ pd
    facs2 = ("Length facs: " ++ show (length facs) ++ "\n") `trace` facs
    psss = map (facToPartialStrengths fund maxFreq) facs2
    divisor = if length facs == 0
              then throwMine $ printf
                   ("No freq/ampl curves (FACs) were present in time " ++
                    "range %.2f ms to %.2f ms.") (1000*t1) (1000*t2)
              else fromIntegral $ length facs
    u = M.unionsWith (+) psss

-- timeRangeToGen10Table
--  
-- Uses -10, not 10, so that table is not normalized
--       
-- Inputs
--   PvsDump
--   Float :: fundamental freq
--   Float :: max frequency of a partial
--   Float :: sampling rate
--   Float :: t1 (lower time)
--   Float :: t2 (higher time)
-- Outputs
--   String :: gen 10 table (not including "f<n> 0 <size>"
--  
timeRangeToGen10Table :: PvsDump -> Float -> Float -> Float -> Float ->
                         Float -> String
timeRangeToGen10Table pd fund maxFreq sampRate t1 t2 = 
  "-10 " ++ partials ++ "\n"
  where
    s = timeRangeToPartialStrengths pd fund maxFreq sampRate t1 t2
    s2 = ("In timeRangeToGen10Table: " ++ show s ++ "\n") `trace` s
    partials = concatMap (\(_,v) -> printf " %.5f" v) . M.toAscList $ s2
      

showPvsDump :: PvsDump -> String
showPvsDump pd = printf "Number of FACs in PvsDump: %d" (M.size pd)


main = do
  -- CONFIGURATION
  let soundFileDir = "/giga/vienna/18-Clarinet-Bb/KLB_PERF-LEGATO-waves/"
      soundFile = soundFileDir ++ "KLB_pA_sus_mp_C4.wav"
      pvsDataDir = "/Temp/csound/tables/clarinet-C4"
      samplingRate = 44100
      maxFTablePartialFreq = 18000
      soundFileFundmental = 261.66  -- Hertz
      analysisDur = 0.250
      -- analysis t1 and analysis t2: 
      --   we will examine the partials that fall between T1 and T2
      --   (and average them), in order to create an f-table to imitate them
      t1 =  0.200 -- seconds
      t2 =  0.250
      fTableFileName = "f-table.csd"

  -- Step 1: pvs analysis
  pvsAnalysis soundFile pvsDataDir samplingRate soundFileFundmental analysisDur

  -- Step  : read pvs analysis data
  pvsDump <- readPvsDump pvsDataDir 
  putStrLn $ showPvsDump pvsDump

  -- Step  : create an f-table using average values between t1 and t2
  writeFile fTableFileName (timeRangeToGen10Table pvsDump soundFileFundmental
    maxFTablePartialFreq samplingRate t1 t2)
