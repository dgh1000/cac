
import Text.Printf
import qualified Data.Map as M
import Data.Map(Map)
import Util.MiscCsound.CLib.PvsanalInterface
import Util.MiscCsound.CLib.GnuPlotInterface
import Util.Math
import Util.Exception

{-
generateCsoundInstrument :: [(Float,Float)] -> String
generateCsoundInstrument _ = "foo\n"
-}

mkIStatement :: Float -> Float -> [(Float,Float,Float,Map Int Float)] -> String
mkIStatement dur fund rangesCurves = 
  let ampl = 1000 :: Float
      iatt = 0.002 :: Float
      itail = 0.01 :: Float
      {-
      avgRange (x,y) = (x+y)/2
      baseTimes = map (avgRange . fst) rangesCurves
      nTimes = length baseTimes
      lastTime = last baseTimes
      remainingTimes = take 10 [lastTime+0.005, lastTime+0.01 .. ]
      allTimes = take 6 $ baseTimes ++ remainingTimes
      -}
      allTimes = take 6 . map (\(_,_,t,_) -> t) $ rangesCurves
  in unlines $ [ printf "i1 0.1 %f %f %f 0 0 0 0.5" dur ampl fund
               , printf "%f %f " iatt itail 
               , concatMap (printf "%f ") allTimes
               , concatMap (printf "%d ") [(1::Int),2,3,4,5,6,7] 
               ]


-- Input
--
makeCsd :: String -> Float -> [(Float,Float,Float,Map Int Float)] -> IO String
makeCsd templateFileName fund rangesCurves = do
  t <- readFile templateFileName
  let curves = map (\(_,_,_,c) -> c) rangesCurves
  let tables = unlines . zipWith toFTable [1..] $ curves
      noteDur = last $ map (\(_,_,c,_) -> c) rangesCurves
      iStatement = mkIStatement noteDur fund rangesCurves
      mixerDur = noteDur + 0.5 :: Float
  return $ printf t tables iStatement mixerDur

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
      tRanges1 = [ (0.005, 0.01, 4)
                , (0.01 , 0.02, 8)
                , (0.02, 0.040, 12)
                , (0.040, 0.080, 16)
                , (0.080, 0.160, 20)
                , (0.160, 0.320, 24)
                , (0.320, 0.5,   30) 
                ]
      tRanges2 = [ (0.005, 0.010,  1)
                , (0.010, 0.020,  2)
                , (0.020, 0.040,  3)
                , (0.040, 0.080,  4)
                , (0.080, 0.160,  5)
                , (0.160, 0.320,  6)
                , (0.320, 0.500,  7)
                ]
      tRanges3 = [ (0.300, 0.310, 1)
                 , (0.310, 0.320, 2)
                 , (0.320, 0.330, 3)
                 , (0.330, 0.340, 4)
                 , (0.340, 0.350, 5)
                 , (0.350, 0.360, 6)
                 , (0.310, 0.320, 7) ]
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

  csd <- makeCsd "csound-instr-template.csd" soundFileFundmental $
         zipWith (\(a,b,c) d -> (a,b,c,d)) tRanges psss
  writeFile "generated.csd" csd
  -- writeFile "gnuData.txt" gnuData
  -- Write all generated code
  -- writeFile generatedCodeFileName csoundCode

  