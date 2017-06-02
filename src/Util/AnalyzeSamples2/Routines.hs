
import qualified Data.List as L
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import Util.Math
import Util.CLib.PvsanalInterface
import Util.CLib.CLibData( PvsAnalysisChannel(..) )
import Util.Exception

pvsanal :: Int -> String -> String -> PvsAnalysisChannel -> Float -> IO Bool
pvsanal midiPitch dir soundFile channel dur = do
  let sr = 44100
      fund = midiPitchToFreq midiPitch
      (fftsize,overlap,winsize,tablesize) = computePvsanalArgsStyle1 sr
                                            fund
      skipSamples = 0

  (status, msg)<- pvsAnalysis soundFile dir fftsize overlap winsize
                   tablesize (Just fund) channel skipSamples dur
  putStrLn msg
  return status

mkTable width pvsData maxFreq partialWidth sourceTime  = ps
  where
    t1 = sourceTime - width/2
    t2 = sourceTime + width/2
    ps = timeRangeToPartialStrengths pvsData maxFreq 44100 partialWidth t1 t2

mkGnuPlotFileName :: String -> Float -> String
mkGnuPlotFileName basename time = 
  printf "%s%04d.txt" basename (round (time*1000) :: Int)

writeGnuplotData :: String -> (Float,Map Int Float) -> IO ()
writeGnuplotData basename (time,data_) = do
  let filename = mkGnuPlotFileName basename time
      onePoint (x,y) = printf "%d %f" x y
      result = unlines $ map onePoint (M.toList data_)
  writeFile filename result
     
  

main = do
  let soundFile =  "/giga/vienna/18-Clarinet-Bb/chopped/KLB_pA_sus_mp_C4.wav"
      pvsDataRoot = "/Temp/csound/tables/clarinet"
      pvsDataSubPath = "/leg-mp/"
      midiPitch = 60
      dur = 1.5
      whichChannel = PvsLeftChOnly
      maxFreq = 10000
      partialWidth = 0.5  -- call this W. when analyzing the energy in a 
                 -- partial N with fundamental F, include any pvs analysis bin
                 -- within the frequency range (N*F - W/2, N*F + W/2)
      timeAvgWidth = 0.010

      t0   = 0.090    -- where sound begins
      tEnd = 0.700  -- time of end of analysis
      nTimes =  5 -- number of times to analyze
      gnuplotBasename = "plotClarinet"

      pvsDataDir = pvsDataRoot ++ pvsDataSubPath
  pvsanal midiPitch pvsDataDir soundFile whichChannel dur
  putStrLn "\n\nNow reading PvsDump and processing it."
  pvsData <- readPvsDump pvsDataDir
  let d = tEnd - t0
      step = if nTimes < 2 
             then throwMine "nTimes too small"
             else (tEnd - t0)/fromIntegral (nTimes - 1)
      timesList :: [Float]
      timesList = let x = take nTimes $ iterate (+ step) t0
                  in ("\n" ++ show x) `trace` x
         
      tables = map (mkTable timeAvgWidth pvsData maxFreq partialWidth) 
               timesList
      dbTables = map (M.map toDecibels) tables
      norms = map (normalizeTable (last dbTables)) dbTables
      normalizeTable :: Map Int Float -> Map Int Float -> Map Int Float
      normalizeTable = M.unionWith (flip (-))
  mapM_ (writeGnuplotData gnuplotBasename) (zip timesList norms)
  let gnuCmdFilename t= printf " \"%s\" " (mkGnuPlotFileName gnuplotBasename t)
      gnuCmds = "plot " ++ L.intercalate "," 
        (map gnuCmdFilename timesList) ++ "\n"
  writeFile "plotCmds.txt" gnuCmds



  
  