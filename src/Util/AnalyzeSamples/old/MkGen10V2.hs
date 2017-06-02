
import Data.Binary
import Data.ByteString.Lazy
import Csound.Instruments.Gen10ImitateSamplerV2

-- config
onePitchDataFile = "onePitch_60.bin"
pvsDataRoot = "/Temp/csound/tables/clarinet"


----------------------------------------------------------------------
----------------------------------------------------------------------
-- sub-functions
----------------------------------------------------------------------
----------------------------------------------------------------------

computePvsanalArgsStyle1 :: Float -> Float -> (Int,Int,Int,Int)
computePvsanalArgsStyle1 sr soundFileFundmental =
  (fftsize,overlap,winsize,tablesize)
  where
    fftsize = roundToEven $ 2*sr/soundFileFundmental
    overlap = ceiling $ fromIntegral fftsize / (4 :: Float)
    winsize = fftsize
    tablesize = roundUpPowerOfTwo fftsize


pvsanal :: Int -> String -> PvsAnalysisChannel -> Float -> IO Bool
pvsanal midiPitch subPath channel dur = do
  let sr = 44100
      fund = UM.midiPitchToFreq midiPitch
      pvsDataDir = pvsDataRoot ++ subPath ++ "/" ++ show midiPitch ++ "/"
      (fftsize,overlap,winsize,tablesize) = computePvsanalArgsStyle1 sr
                                            fund
      skipSamples = 0

  (status, msg) <- pvsAnalysis fileName pvsDataDir fftsize overlap winsize 
                   tablesize (Just fund) channel skipSamples dur
  putStrLn msg


----------------------------------------------------------------------
----------------------------------------------------------------------
-- main functions
----------------------------------------------------------------------
----------------------------------------------------------------------

mkDefaultConfig = do
  let wa = WaveformAttack 0 [] []
      wr = WaveformRelease 0 [] []
      op = OnePitch wa wr wa wa wr
  b = encode op
  BL.writeFile onePitchDataFile b




