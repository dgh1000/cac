{-

Utilities for 

  (1) generating code for a realtime Csound synthesizer, specifically f
  statements

  (2) running a program to generate the samples

-}

import Text.Printf
import Control.Monad
import System.Process


-- Generate a separate sample wav file for every pitch.
genSamples :: (Int -> (String,String)) -> String -> Int -> Int -> 
              IO ()
genSamples f orcName loPit hiPit = do
  let g :: Int -> IO ()
      g pit = do
        let (filename,scoreContents) = f pit
        writeFile "score.sco" scoreContents
        system $ printf "csound -W -o %s %s score.sco" filename orcName
        return ()
  forM_ [loPit..hiPit] g


midiPitchToCps :: Int -> Float
midiPitchToCps p = 440 * (2 ** (((fromIntegral p)-69)/12))

genPinkSamples = do
  let lowPitch = 21
      highPitch = 108
      orcName = "/haskell/Util/softsynth/gen-pink.orc"
      duration = 4.0 :: Double

      f :: Int -> (String,String)
      -- take midi pitch, and return sample file name and sample-generating
      -- score
      f pit = 
       ( printf "\\Users\\Mike\\samples\\pink\\pink%d.wav" pit
       , printf "i1 0 %.3f 20000 %.3f\n" duration (midiPitchToCps pit))
  genSamples f orcName lowPitch highPitch


----------------------------------------------------------------------
----------------------------------------------------------------------
--    second method: one big file containing all samples


computeGenPinkScore :: Integer -> Int -> Double -> Int -> Int -> String
computeGenPinkScore tableSize ampl delta lowPitch highPitch = 
  (unlines $ map g [lowPitch..highPitch]) ++ finalLine
  where
    dur      = fromIntegral (highPitch-lowPitch+1) * delta :: Double
    tableDur = fromIntegral tableSize / 44100 :: Double
    finalLine =  
      printf "i1 %.3f %.3f %d %.3f" dur (tableDur-dur+2) ampl (440.0::Double)

    g :: Int -> String
    g p = printf "i1 %.3f %.3f %d %.3f" tBeg (delta-0.001) ampl freq
      where 
        tBeg = fromIntegral (p-21) * delta
        freq = midiPitchToCps p


-- Generate one wav file that contains all samples.
genPinkSamples2 = do
  -- configuration
  let duration   = 3.5
      lowPitch   = 21
      highPitch  = 108
      tableSize  = 2^24 :: Integer 
      ampl       = 20000
      orcName    = "/haskell/Util/softsynth/gen-pink.orc"

      -- begin processing
      sc = computeGenPinkScore tableSize ampl duration lowPitch highPitch
  writeFile "sco.sco" sc
  system $
    printf 
    "csound -W -o \\Users\\Mike\\samples\\pink\\pink-samples.wav %s sco.sco"
     orcName

----------------------------------------------------------------------
----------------------------------------------------------------------
--             write synth.orc


writeOrc = do
  let laptop = True
      orcTemplate  = "synth-template.csd"
      orc          = "synth.csd"
      -- laptop
      --   dac7 is iFi driver
      outputDevice = if laptop then "dac7" else ""
      -- laptop
      --   2 is internal MIDI 03
      --   3 is internal MIDI 03
      midiDevice   = if laptop then 2 else 3  :: Int
      sampleDuration = 3.5      :: Double
      sampleOffset   = 3.0      :: Double
      tableSize      = 2^24 :: Integer -- 16777216

  c <- readFile orcTemplate 
  let c2 = printf c outputDevice midiDevice sampleDuration sampleOffset 
           tableSize 
  writeFile orc c2




main = writeOrc
