module Util.AnalyzeSamples.Execute where

import Debug.Trace
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import Util.ParseBracketed
import Util.CLib.CLibData( PvsAnalysisChannel(..) )
import Util.Exception
import Util.Math( roundToEven 
                , roundUpPowerOfTwo )
import qualified Util.ParseBracketed as PB
import Util.ParseBracketed( SquareBracketed )
import Util.CLib.PvsanalInterface( pvsAnalysis 
                                 , readPvsDump
                                 , timeRangeToPartialStrengths )
import Csound.Instruments.Gen10ImitateSampler
import Csound.Instruments.InstrConfigPath as ICP

----------------------------------------------------------------------
----------------------------------------------------------------------
-- Map of bracket-group name to function for executing it.
----------------------------------------------------------------------
----------------------------------------------------------------------

execMap :: Map String (SquareBracketed -> IO ())
execMap = M.fromList [ ("simple-a", execSimpleA)
                     , ("instr-353-ratio", execInstr353Ratio) ]

----------------------------------------------------------------------
----------------------------------------------------------------------
-- General-purpose routines
----------------------------------------------------------------------
----------------------------------------------------------------------


toPvsChannel "left" = PvsLeftChOnly
toPvsChannel "right" = PvsRightChOnly
toPvsChannel "average" = PvsAverageChs
toPvsChannel s = throwMine $ printf ("In config, channel was given as '%s';"++
                 " this is not recognizable. Acceptable entries are " ++
                 "'left', 'right', and 'average'") s
 

computePvsanalArgsStyle1 :: Float -> Float -> (Int,Int,Int,Int)
computePvsanalArgsStyle1 sr soundFileFundmental =
  (fftsize,overlap,winsize,tablesize)
  where
    fftsize = roundToEven $ 2*sr/soundFileFundmental
    overlap = ceiling $ fromIntegral fftsize / (4 :: Float)
    winsize = fftsize
    tablesize = roundUpPowerOfTwo fftsize

----------------------------------------------------------------------
----------------------------------------------------------------------

{-        simple-a       pvs analysis of a single file

[ simple-a

  (- other params like fftsize are computed automatically in style 1)
  (soundFileDir    "/giga/vienna/18-Clarinet-Bb/KLB_PERF-LEGATO-waves/")
  (soundFileName    "KLB_pA_sus_mp_C4.wav")
  (pvsDataDir       "/Temp/csound/tables/clarinet-C4")
  (samplingRate     44100)
  (soundFileFundamental  261.66)
  (-- initial time of waveform to skip in loading it, in seconds)
  (skip 0)
  (dur 0.5)

]

-}
execSimpleA :: SquareBracketed -> IO ()
execSimpleA sb@(SquareBracketed _ fields) = do
  let sr = PB.lookupFloat "samplingRate" sb
      fund = PB.lookupFloat "soundFileFundamental" sb
      pvsDataDir = PB.lookupString "pvsDataDir" sb
      fileName = PB.lookupString "soundFileDir" sb ++ "/" ++
                 PB.lookupString "soundFileName" sb
      channel = toPvsChannel $ PB.lookupString "channel" sb
      skip = PB.lookupFloat "skip" sb
      dur = PB.lookupFloat "dur" sb
      (fftsize,overlap,winsize,tablesize) = computePvsanalArgsStyle1 sr
                                            fund
      skipSamples = round (skip * sr) :: Int
  (status, msg) <- pvsAnalysis fileName pvsDataDir fftsize overlap winsize 
                   tablesize (Just fund) channel skipSamples dur
  putStrLn msg




----------------------------------------------------------------------
----------------------------------------------------------------------
{-       simple-b       configure Gen10ImitationSampler instrument
                        from reslts of pvs analysis
   NOW OBSOLETE: see simple-b-ratio
   (THIS IS OBSOLETE because we changed how we handle tail time, so this
    no longer functions with instr 353)

[ simple-b

  (pvs-data-dir       "/Temp/csound/tables/clarinet-C4")

  (fundamental          264)

  (-- width of time window for averaging source sound file spectrum in seconds)
  (width 0.010)

  (- spectrum time: for each triplet of times, the meaning is
            <source spectrum time>
            <playback-time>
     this spectrum is played back starting at <playback-time>.
     Note: the playback-time in spectrum-time-0 is ignored; in actual 
     playback that goes to time 0. )
  (--- initial spectrum; playback time is ignored )      
  (spectrum-time-0   0.010 0.000)
  (--- intermediate attack spectrums )
  (spectrum-time-1   0.100 0.100)
  (spectrum-time-2   0.300 0.300)
  (--- final steady-state spectrum )
  (spectrum-time-3   0.500 0.500 0.500)
  (--- spectrum: goal for end of tail; playback time is ignored )
  (spectrum-time-4   0.600 0.600)

  (-- initial linear ampl attack )  
  (initial-attack 0.003)
  (sample-rate 44100)
  (-- maximum frequency for any partial to include)
  (maxFreq 10000)
  (-- default length of tail)
  (tail 0.30)
]

-}

{-
execSimpleB :: SquareBracketed -> IO ()
execSimpleB sb@(SquareBracketed _ fields) = do
  let lookup2Floats s = let fs = PB.lookupMany1Floats s sb in 
        if length fs == 2 
          then fs 
          else throwMine "in executing simple-b group, expected 2 float values"

      dir = PB.lookupString "pvs-data-dir" sb
      st0 = lookup2Floats "spectrum-time-0"
      st1 = lookup2Floats "spectrum-time-1"
      st2 = lookup2Floats "spectrum-time-2"
      st3 = lookup2Floats "spectrum-time-3"
      st4 = lookup2Floats "spectrum-time-4"
      st5 = lookup2Floats "spectrum-time-5"
      st6 = lookup2Floats "spectrum-time-6"
      maxFreq = PB.lookupFloat "maxFreq" sb
      sampRate = PB.lookupFloat "sample-rate" sb
      attDur = PB.lookupFloat "initial-attack" sb
      width = PB.lookupFloat "width" sb
      tail = PB.lookupFloat "tail" sb


  pvsData <- readPvsDump dir
  let mkTable [sourceTime,_] = 
        let t1 = max 0 (sourceTime - width/2)
            t2 = sourceTime + width/2
            ps =timeRangeToPartialStrengths pvsData maxFreq sampRate t1 t2
            harms :: [Float]
            harms = map snd (M.toAscList ps)
        in "2048 -10 " ++ concatMap (printf " %f") harms
      table0 = mkTable st0
      table1 = mkTable st1
      table2 = mkTable st2
      table3 = mkTable st3
      table4 = mkTable st4
      table5 = mkTable st5 -- steady state specturm
      table6 = mkTable st6
      d= Gen10ImitateSamplerData attDur (st5 !! 1) 
         [st1 !! 1, st2 !! 1, st3 !! 1, st4 !! 1, st5 !! 1]
         [table0, table1, table2, table3, table4, table5] table6 tail
      b = encode d
  BL.writeFile (ICP.path ++ gen10ImitateSamplerFileName) b
  putStrLn "\n\nDone creating gen10ImitateSampler config."
-}

{-

----------------------------------------------------------------------
----------------------------------------------------------------------
--        instr-353-ratio
--              
--
--  For use with instr 353 -- see the chart there for explanation
--  of how tables and times interact.

[ instr-353-ratio

  (pvs-data-dir       "/Temp/csound/tables/clarinet-C4")

  (- the following two fields are the widths of the time window to average 
     source spectrum)

  (- width at t1. This will be a little smaller since the spectrum is
     rapidly changing there)
  (width-1 0.010)

  (- width at all other times)
  (width-normal 0.020)

  (-- more or less where the sound starts in the source file)
  (source-time-0 0.070)

  (--- The time at which spectrum 0 is sampled in the source. )
  (source-time-1   0.080)

  (-- note: in this config, we skip source times 2, 3, and 4, which 
      the program will automatically position

  (--- The time at which spectrum 4 is sampled in the source.
       Note: spectrum 5 is the steady-
       state spectrum, or put another way, at the very end of the attack 
       portion.)
  (source-time-5   0.800)

  (--- This is the end of the steady-state portion of the source.)
  (source-time-6   2.210)

  (--- The sample time of spectrum 5, which is at t7 in the playback. 
       This is the first significant change after the start of the decay.)
  (source-time-7   2.250)

  (--- The sample time of spectrum 6, which is at t8 in the playback.
       This is the final spectrum, the one that gets faded to silence.)
  (source-time-8   2.300)

  (-- the time when the source sound goes silent)
  (source-dur      2.600)

  (sample-rate 44100)
  (-- maximum frequency for any partial to include)
  (max-freq 10000)
]

-}

execInstr353Ratio :: SquareBracketed -> IO ()
execInstr353Ratio sb = do
  let pvsDataDir = PB.lookupString "pvs-data-dir" sb
      width1 = PB.lookupFloat "width-1" sb
      widthNormal = PB.lookupFloat "width-normal" sb
      sourceTime0 = PB.lookupFloat "source-time-0" sb
      sourceTime1 = PB.lookupFloat "source-time-1" sb
      sourceTime5 = PB.lookupFloat "source-time-5" sb
      sourceTime6 = PB.lookupFloat "source-time-6" sb
      sourceTime7 = PB.lookupFloat "source-time-7" sb
      sourceTime8 = PB.lookupFloat "source-time-8" sb
      sourceDur   = PB.lookupFloat "source-dur" sb
      sr = PB.lookupFloat "sample-rate" sb
      maxFreq = PB.lookupFloat "max-freq" sb

      -- computed times
      delta = sourceTime6 - sourceTime5
      sourceTime2 = sourceTime1 + 1*delta/4
      sourceTime3 = sourceTime1 + 2*delta/4
      sourceTime4 = sourceTime1 + 3*delta/4
  pvsData <- readPvsDump pvsDataDir
  let mkTable sourceTime width = 
        let t1 = sourceTime - width/2
            t2 = sourceTime + width/2
            ps = timeRangeToPartialStrengths pvsData maxFreq sr t1 t2
            harms :: [Float]
            harms = map snd (M.toAscList ps)
        in "2048 -10 " ++ concatMap (printf " %f") harms
      table0 = mkTable sourceTime1 width1
      table1 = mkTable sourceTime2 widthNormal
      table2 = mkTable sourceTime3 widthNormal
      table3 = mkTable sourceTime4 widthNormal
      table4 = mkTable sourceTime5 widthNormal
      table5 = mkTable sourceTime7 widthNormal
      table6 = mkTable sourceTime8 widthNormal
      d = Gen10ImitateSamplerData 
          (sourceTime1 - sourceTime0)
          (sourceTime2 - sourceTime0)
          (sourceTime3 - sourceTime0)
          (sourceTime4 - sourceTime0)
          (sourceTime5 - sourceTime0)
          (sourceTime6 - sourceTime0)
          (sourceTime7 - sourceTime0)
          (sourceTime8 - sourceTime0)
          (sourceDur - sourceTime0)
          [table0, table1, table2, table3, table4, table5, table6]
  BL.writeFile (ICP.path ++ gen10ImitateSamplerFileName) (encode d)
  putStrLn "\n\nDone creating gen10ImitateSampler config."
   
{-      
execInstr353Ratio :: SquareBracketed -> IO () 
execInstr353Ratio sb = do
  let d = Gen10ImitateSamplerData
          1 2 3 4 5 6 7 8 9
          [ "8192 -10 32000"
          , "8192 -10 32000 32000 32000 32000 32000 32000 32000"
          , "8192 -10 32000"
          , "8192 -10 32000 32000 32000 32000 32000 32000 32000"
          , "8192 -10 32000"
          , "8192 -10 32000 32000 32000 32000 32000 32000 32001"
          , "8192 -10 32000"
          ]
  BL.writeFile (ICP.path ++ gen10ImitateSamplerFileName) (encode d)
  putStrLn "\n\nDone creating gen10ImitateSampler config."
 
-}


{-
----------------------------------------------------------------------
----------------------------------------------------------------------
--        instr-353-v2
--              
--
--  For 
--  For use with instr 353 -- see the chart there for explanation
--  of how tables and times interact.

[ instr-353-ratio

  (pvs-data-dir       "/Temp/csound/tables/clarinet-C4")

  (- the following two fields are the widths of the time window to average 
     source spectrum)

  (- width at t1. This will be a little smaller since the spectrum is
     rapidly changing there)
  (width-1 0.010)

  (- width at all other times)
  (width-normal 0.020)

  (-- more or less where the sound starts in the source file)
  (source-time-0 0.070)

  (--- The time at which spectrum 0 is sampled in the source. )
  (source-time-1   0.080)

  (-- note: in this config, we skip source times 2, 3, and 4, which 
      the program will automatically position

  (--- The time at which spectrum 4 is sampled in the source.
       Note: spectrum 5 is the steady-
       state spectrum, or put another way, at the very end of the attack 
       portion.)
  (source-time-5   0.800)

  (--- This is the end of the steady-state portion of the source.)
  (source-time-6   2.210)

  (--- The sample time of spectrum 5, which is at t7 in the playback. 
       This is the first significant change after the start of the decay.)
  (source-time-7   2.250)

  (--- The sample time of spectrum 6, which is at t8 in the playback.
       This is the final spectrum, the one that gets faded to silence.)
  (source-time-8   2.300)

  (-- the time when the source sound goes silent)
  (source-dur      2.600)

  (sample-rate 44100)
  (-- maximum frequency for any partial to include)
  (max-freq 10000)
]

-}
