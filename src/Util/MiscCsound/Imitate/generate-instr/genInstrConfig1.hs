
{-

okay we want to make an instrument we can use with music xml dumps

-}

import Data.Binary
import Text.Printf
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.ByteString.Lazy as BL
import Util.MiscCsound.CLib.PvsanalInterface
import Util.MiscCsound.CLib.GnuPlotInterface
import Util.Math
import Util.Exception
import Csound.CsoundData( OneLoadedInstrConfig(..)
                        , LoadedInstrConfig(..) )
import Util.MiscCsound.CLib.LoadedInstrConfigPath(loadedInstrConfigPath)

{-
generateCsoundInstrument :: [(Float,Float)] -> String
generateCsoundInstrument _ = "foo\n"
-}


config1 = LoadedInstrConfig $ M.fromList  [("foo", config2)]
config2 = Gen10ImitateSamplerLoadedConfig 0.1 4.0
          [1, 2, 3]
          [ "8192 10 1", "8192 10 1 1 1 1 1 1"
          , "8192 10 1", "8192 10 1 1 1 1 1 1" ]
          "8192 10 1"

toFTable :: Int -> Map Int Float -> String
toFTable i c = printf "f%d 0 8192 -10 %s" i (concatMap g $ M.toAscList c)
  where 
    g :: (Int,Float) -> String
    g (_,v) = printf " %f" v

main = do
  -- CONFIGURATION
  let soundFileDir = "/giga/vienna/18-Clarinet-Bb/KLB_PERF-LEGATO-waves/"
      soundFile = soundFileDir ++ "KLB_pA_sus_mp_C4.wav"
      pvsDataDir = "/Temp/csound/tables/clarinet-C4"
      samplingRate = 44100
      maxFTablePartialFreq = 10000
      soundFileFundmental = 261.66  -- Hertz
      analysisDur = 2.0
      -- analysis time ranges and playback t points
      --   at the moment this will be hard-coded to 7 points for
      --   7 tables, and last playback time is overall duration
      tRanges4 = [ (0.02, 0.040, 0.04)
                 , (0.040, 0.080, 0.08)
                 , (0.080, 0.160, 0.16)
                 , (0.160, 0.320, 0.320)
                 , (0.320, 0.640, 0.640) 
                 , (0.640, 0.99, 0.99)
                 , (0.99, 1.99, 1.99)
                ]
      tRanges = tRanges4
      generatedCodeFileName = "code.txt"
  
  -- Generate csound instrument
  -- let csoundCode = generateCsoundInstrument tRanges


  {-


  -- Run pvsanal
  pvsAnalysis soundFile pvsDataDir samplingRate soundFileFundmental analysisDur
 
  -- Read pvs data
  putStrLn $"Now reading PVS data (if you just pressed 'n', this will read\n"++
           "data from the last time the PVS analysis ran)"
  dump <- readPvsDump pvsDataDir

  -- For each time range make a partial strength set (PSS)
  let psss = map doOnePss tRanges
      doOnePss (x,y,_) = timeRangeToPartialStrengths dump soundFileFundmental 
                 maxFTablePartialFreq samplingRate x y
      -- gnuData = partialCurveToData . M.map toDecibels $ partials

  -}

  BL.writeFile loadedInstrConfigPath $ encode config1
  -- writeFile "gnuData.txt" gnuData
  -- Write all generated code
  -- writeFile generatedCodeFileName csoundCode

  