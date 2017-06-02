module Util.AnalyzeSamples.Lib where

import Data.Maybe
import System.FilePath
import System.Directory
import Debug.Trace
import qualified Data.Map as M
import Data.Map(Map)
import Text.Printf
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Util.CLib.CLibData( PvsAnalysisChannel(..)
                         , PvsDump(..) )
import Util.Math
import Util.CLib.PvsanalInterface
import Util.Exception
import Csound.Instruments.Gen10( OnePitch(..) )

{-
computePvsanalArgsStyle1 :: Float -> Float -> (Int,Int,Int,Int)
computePvsanalArgsStyle1 sr soundFileFundmental =
  (fftsize,overlap,winsize,tablesize)
  where
    fftsize = roundToEven $ 2*sr/soundFileFundmental
    overlap = ceiling $ fromIntegral fftsize / (4 :: Float)
    winsize = fftsize
    tablesize = roundUpPowerOfTwo fftsize
-}

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

computeAttackTimes :: Float -> Float -> (Float,Float,Float)
computeAttackTimes t1 t5 =
  let d = t5-t1 
      t2 = t1 + d/4
      t3 = t1 + 2*d/4
      t4 = t1 + 3*d/4
  in printf "attack times, t1=%f, t2=%f, t3=%f, t4=%f, t5=%f"
     t1 t2 t3 t4 t5 `trace` (t2,t3,t4)

mkSpec :: Float -> Float -> PvsDump -> Float -> Float -> Map Int Float
mkSpec sourceTime width pvsData maxFreq partialWidth =
  let t1 = sourceTime - width/2
      t2 = sourceTime + width/2
  in timeRangeToPartialStrengths pvsData maxFreq 44100 partialWidth
           t1 t2

{-
mkTable sourceTime width pvsData maxFreq partialWidth =
  let t1 = sourceTime - width/2
      t2 = sourceTime + width/2
      ps = timeRangeToPartialStrengths pvsData maxFreq 44100 partialWidth
           t1 t2
      harms :: [Float]
      harms = map snd (M.toAscList ps)
  in "2048 -10 " ++ concatMap (printf " %f ") harms
-}

-- release spectrums for instr 353
--
-- note that the three returned spectrums are considered s4, s5, and s6
--   (s4 is the steady-state)
mkRelSpecs353 t6 t7 t8 timeWidth maxFreq partialWidth pvsData  =
     ( mkSpec t6 timeWidth pvsData maxFreq partialWidth
     , mkSpec t7 timeWidth pvsData maxFreq partialWidth
     , mkSpec t8 timeWidth pvsData maxFreq partialWidth )

-- attack spectrums for instr 353
mkAttSpecs353 t1 t5 timeWidthT1 timeWidth maxFreq partialWidth pvsData =
  let (t2,t3,t4) = computeAttackTimes t1 t5
  in (t2,t3,t4
     , mkSpec t1 timeWidthT1 pvsData maxFreq partialWidth
     , mkSpec t2 timeWidth pvsData maxFreq partialWidth
     , mkSpec t3 timeWidth pvsData maxFreq partialWidth
     , mkSpec t4 timeWidth pvsData maxFreq partialWidth
     , mkSpec t5 timeWidth pvsData maxFreq partialWidth )




rebuildOnePitch :: [FilePath] -> Int -> IO (Maybe (Int,OnePitch))
rebuildOnePitch contents pitch = do
  let filt s = 
        let (base,ext) = splitExtension s 
            pitchRelated = take 3 base == printf "m%2d" pitch
        in pitchRelated && ext == ".spc"

      pitchRelatedFiles = filter filt contents
      myFind pattern = do
        let fs = filter ((== (reverse pattern)) . 
                         take (length pattern) .  
                         reverse . 
                         dropExtension) pitchRelatedFiles
        if length fs == 1
          then BL.readFile (head fs)
          else throwMine $ printf ("Problem: either 0 or more than " ++
                "file of type '%s' found. %s") pattern (show pitchRelatedFiles)
        

  if length pitchRelatedFiles == 0
    then return Nothing
    else do 
      sasData <- myFind "_sas" 
      srsData <- myFind "_srs"
      fasData <- myFind "_fas"
      ssData <- myFind "_ss"
      return $  Just $ (pitch, 
                     OnePitch (decode ssData) (decode sasData) (decode fasData)
                    (decode srsData)
                    )
-- Inputs
--
-- String:: Filename  of Gen10 configuration file, such as 
--    "<fullpath>gen10Violin.bin"
rebuildGen10Config :: String -> IO ()
rebuildGen10Config filename = do
  cwd <- getCurrentDirectory
  contents <- getDirectoryContents cwd
  stuff <- mapM (rebuildOnePitch contents) [1..100]
  let gen10Data = M.fromList . catMaybes $ stuff
  BL.writeFile filename (encode gen10Data)
