
import Text.Printf
import Control.Monad
import System.Process
import Control.Concurrent



midiPitchToCps :: Int -> Float
midiPitchToCps p = 440 * (2 ** (((fromIntegral p)-69)/12))

----------------------------------------------------------------------
----------------------------------------------------------------------
genScoFStatements :: IO ()
genScoFStatements = do
  let g :: Int -> String
      g p = printf "f%d 0 262144 -1 \"c:\\Users\\Mike\\samples\\pink\\pink%d.wav\" 0.0 0 0" p p
  writeFile "temp.sco" $ unlines (map g [21..108])



----------------------------------------------------------------------
----------------------------------------------------------------------
--           first method: separate files

computeGenPink1Sco :: Int -> Double -> Int -> String
computeGenPink1Sco ampl dur pitch =
  printf "i1 0 %.3f %d %.4f\n" dur ampl freq
  where
    freq = midiPitchToCps pitch


genSamples1 = do
  let ampl      = 20000
      dur       = 6.0
      lowPitch  = 83
      highPitch = 83
      sampleDir = "\\Users\\Mike\\samples\\pink"
      g :: Int -> IO ()
      g p = do
        writeFile "sco.sco" $ computeGenPink1Sco ampl dur p
        system $ printf 
          "csound -W -o%s\\pink%d.wav gen-pink.orc sco.sco" sampleDir p
          -- "csound -W -odac7 gen-pink.orc sco.sco"
        threadDelay 1000000
  forM_ [lowPitch..highPitch] g

----------------------------------------------------------------------
----------------------------------------------------------------------
--    second method: one big file containing all samples



computeGenPinkScore :: Int -> Int -> String
computeGenPinkScore lowPitch highPitch = 
  (unlines $ map g [lowPitch..highPitch]) -- ++ finalLine
  where
    -- configuration
    delay     = 6
    signalDur = 0.5
    
    g :: Int -> String
    g p = printf "i1 %.3f %.3f %.3f %.3f" tBeg (delay+signalDur) delay freq
      where 
        tBeg = fromIntegral (p-lowPitch) * signalDur :: Double
        freq = midiPitchToCps p 


-- Generate one wav file that contains all samples.
genPinkSamples = do
  -- configuration
  let lowPitch   = 21
      highPitch  = 108
      orcName    = "/haskell/Util/softsynth/gen-pink.orc"
      -- begin processing
      sc = computeGenPinkScore lowPitch highPitch
  writeFile "sco.sco" sc
  system $
    printf 
    "csound -W -o \\Users\\Mike\\samples\\pink\\pink-samples.wav %s sco.sco"
     orcName

main = genPinkSamples
