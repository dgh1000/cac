-- module Util.MiscCsound.HetroFormat.GenCsdMyHetro where

import Text.Printf


headerCode = [ "<CsoundSynthesizer>"
             , "<CsOptions>"
             , "-oout.wav -W"
             , "</CsOptions>"
             , "<CsInstruments>"
             , "sr=44100"
             , "ksmps=10"
             , "nchnls=2"
             , "gi_sin ftgen 0, 0, 8192, 10, 1" ]

middleCode = [ "</CsInstruments>"
             , "<CsScore>" ]

endCode = [ "</CsScore>"
          , "</CsoundSynthesizer>"]


-- global table declaration for one partial: 
--   parameters are partial number append to name,
--   table length, table length
globTabCode = "gi_outTab%d ftgen 0, 0, %d, -7, 0, %d, 0"

cfCode = "gi_cf = %f\n"

-- trackingInstrument
--
-- String :: file name of wav file
-- Float :: fundamental
-- Int :: number of harmonics (includ fundamental)
-- Float :: bandwidth of tracking filters
-- Float: to get low pass cutoff, divide the partial's freq by this
-- Float
-- 
trackingInstrument :: String -> Float -> Int -> Float -> Float -> String
trackingInstrument fname fund n bw lowPassDivide =
  "instr\n asigNull init 0\n asigIn init 0\n"
  ++ "fin \"" ++ fname ++ "\", 0, 0, asigNull, asigIn\n"
  ++ resons ++ absolutes ++ lowPasses 
  ++ "aidx phasor sr/ftlen(gi_outTab1)\n" ++ tableWrites ++ "endin\n"
  where
    freqList = zip [1..n] $ map ((*fund) . fromIntegral) [1..n]
    resons = concatMap oneReson freqList
    absolutes = concatMap oneAbs freqList
    lowPasses = concatMap oneLow freqList
    tableWrites = concatMap oneWrite freqList
    oneReson (i,f) = printf "afilt%d reson asigIn, %f, %f, 1\n" i f bw
    oneAbs (i,_) = printf "afilt%da = abs(afilt%d)\n" i i
    oneLow (i,f) = 
        printf "atrack%d butterlp afilt%da, %f\n" i i (f/lowPassDivide)
    oneWrite (i,_) = printf "tablew atrack%d, aidx, gi_outTab%d, 1, 0, 1\n" i i

-- playbackInstrument
--
-- Float :: fundamental
-- Int :: # of partials including fundamental
--
playbackInstrument :: Float -> Int -> String
playbackInstrument fund n = "instr\nifreq=sr/ftlen(gi_outTab1)\n" 
  ++ oscils ++ aout ++ "outs aout, aout\n endin\n"
  where
    fs = zip [1..n] $ map ((*fund) . fromIntegral) [1..2]
    oscils = concatMap oneOscil fs
    aout = "aout = asig1" 
           ++ concatMap (\i -> printf "+asig%d" i) [2..n] ++ "\n"
    oneOscil (i,f) = 
      printf
      "aenv%d oscili 1, ifreq, gi_outTab%d\nasig%d oscili 1, %f, gi_sin\n"
      i i i f




main = do
  let fundamental = 262
      numPartials = 3
      trackingBandwidth = 262/3
      lowPassDivide = 10
      filename = ""

  let s = trackingInstrument filename fundamental numPartials 
          trackingBandwidth lowPassDivide
  let s2 = playbackInstrument fundamental numPartials
  writeFile "test.txt" s2

    

{-

----------------------------------------------------------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
----------------------------------------------------------------------
-- code for tracking instrument
----------------------------------------------------------------------
----------------------------------------------------------------------

instr

asigNull init 0
asigIn init 0            ; fundamental


fin "../HetroFormat/pno-C4-trim.wav", 0, 0, asigNull, asigIn
afilt1 reson asigIn, 262, 131, 1   ; fundamental
afilt2 reson asigIn, 524, 131, 1   ; partial

afilt1a = abs(afilt1)
afilt2a = abs(afilt2)

atrack1 butterlp afilt1a, 262/10
atrack2 butterlp afilt2a, 524/10

aidx phasor sr/ftlen(gi_outTab1)

     tablew atrack1, aidx, gi_outTab1, 1, 0, 1
     tablew atrack2, aidx, gi_outTab2, 1, 0, 1

endin


----------------------------------------------------------------------
----------------------------------------------------------------------
-- code for playback
----------------------------------------------------------------------
----------------------------------------------------------------------

instr

ifreq = sr/<table length>

aenv1 oscili 1, ifreq, gi_outTab1
asig1 oscili 1, 262, gi_sin
 
aenv2 oscili 1, ifreq, gi_outTab2
asig2 oscili 1, 524, gi_sin

aout = asig1+asig2+...

   outs aout,aout
 


-}
