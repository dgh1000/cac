-- module Util.MiscCsound.HetroFormat.GenCsdMyHetro where

import Text.Printf

import Util.Math

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
-- String :: input file name
-- Float :: fundamental
-- Int :: number of harmonics (includ fundamental)
-- Float :: bandwidth of tracking filters
-- Float: to get low pass cutoff, divide the partial's freq by this
-- Float
-- 
trackingInstrument :: String -> Float -> Int -> Float -> Float -> String
trackingInstrument fn fund n bw lowPassDivide =
  "instr 1\n asigIn1 init 0\n asigIn2 init 0\n"
  ++ "fin \"" ++ fn ++ " \", 0, 0, asigIn1, asigIn2\n"
  ++ "asigIn = asigIn1 + asigIn2\n"
  ++ resons ++ absolutes ++ lowPasses 
  ++ "aidx phasor sr/ftlen(gi_outTab1)\n" ++ tableWrites 
  ++ "outs asigIn1, asigIn2\nendin\n"
  where
    freqList = zip [1..n] $ map ((*fund) . fromIntegral) [1..n]
    resons = concatMap oneReson freqList
    absolutes = concatMap oneAbs freqList
    lowPasses = concatMap oneLow freqList
    tableWrites = concatMap oneWrite freqList
    oneReson (i,f) = 
      let nResonsRaw = round $ f/fund/20 :: Int
          nResons = min 10 (max 1 nResonsRaw)
      in printf "afilt%d resonx asigIn, %f, %f, %d, 1\n" i f bw nResons
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
playbackInstrument fund n = "instr 2\nifreq=sr/ftlen(gi_outTab1)\n" 
  ++ oscils ++ aout ++ "outs aout, aout\n endin\n"
  where
    fs = zip [1..n] $ map ((*fund) . fromIntegral) [1..n]
    oscils = concatMap oneOscil fs
    aout = "aout = asig1" 
           ++ concatMap (\i -> printf "+asig%d" i) [2..n] ++ "\n"
    oneOscil (i,f) = 
     printf
     "aenv%d oscili %f, ifreq, gi_outTab%d\nasig%d oscili aenv%d, %f, gi_sin\n"
     i (suppressionRatio f) i i i f

{-
suppressionRatio :: Float -> Float
suppressionRatio f 
  | f < 2000 = 1
  | 1000 <= f && f <= 4000 = scale 2000 f 5000 1 0.05
  | otherwise = 0.05
-}

suppressionRatio _ = 1 :: Float

-- globalTables
--
-- Int :: number of partials
-- Int :: table size
globalTables :: Int -> Int -> String
globalTables n s = concatMap oneTable [1..n]
  where
    oneTable i = printf "gi_outTab%d ftgen 0, 0, %d, -7, 0, %d, 0\n"
                 i s s


iStatements :: Float -> String
iStatements d = printf " i1 0 %f\n i2 %f %f\n" d (d+0.5) (d + 0.5)

main = do
  let fundamental = 262
      numPartials = 60
      trackingBandwidth = 262/30
      lowPassDivide = 30
      tableSize = 131072
      trackingDur = 2.5
      csdFilename = "test.csd"
      inFileName = "pno-C4-trim.wav"

  let s = unlines headerCode
          ++ globalTables numPartials tableSize
          ++ trackingInstrument inFileName 
             fundamental numPartials trackingBandwidth
             lowPassDivide
          ++ playbackInstrument fundamental numPartials
          ++ unlines middleCode
          ++ iStatements trackingDur 
          ++ unlines endCode

  writeFile csdFilename s

    

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
