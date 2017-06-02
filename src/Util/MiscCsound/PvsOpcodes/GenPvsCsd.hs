
module Util.MiscCsound.PvsOpcodes.GenPvsCsd where

import Text.Printf

headerCode = unlines
 [ "<CsoundSynthesizer>"
 , "<CsOptions>"
 , "-oout.wav -W"
 , "</CsOptions>"
 , "<CsInstruments>"
 , "sr=44100"
 , "ksmps=10"
 , "nchnls=2"
 , "gi_sin ftgen 0, 0, 8192, 10, 1" ]

middleCode = unlines [ "</CsInstruments>"
                     , "<CsScore>" ]

endCode = unlines [ "</CsScore>"
                  , "</CsoundSynthesizer>"]


-- computeInstrCode
--   Int :: number of harmonics
--   Float :: fundamental frequency
--   Float :: milliseconds per table envelope entry
computeInstrCode :: Int -> Float -> Float -> String
computeInstrCode nParts fund msPerSamp = 
  printf "instr 1\n ifund=%.4f\n" fund
  ++ printf "imsPerSample = %.4f\n" msPerSamp
  ++ "itablen = ftlen(1)\n iT = itablen* imsPerSample/1000\n"
  ++ "iphasor_freq = 1.0 / iT\n"
  ++ "aphs phasor iphasor_freq\n"
  ++ concatMap onePartial [1..nParts]
  ++ "aout = asig1 " ++ concatMap (\n -> "+ asig" ++ show n) [2..nParts] 
  ++ "\n outs aout,aout\n"
  ++ "endin\n"
  where
    onePartial n = 
      printf "aamp%d tablei aphs, %d, 1\n" n n
      ++ printf "asig%d oscili aamp%d, %d*ifund, gi_sin\n" n n n
 

genCsd :: Int -> Float -> Float -> Int -> String -> String
genCsd nParts fund msPerSamp tableLen tablesCode =
  headerCode ++ instr ++ middleCode ++ scoreCode ++ endCode
  where
    instr = computeInstrCode nParts fund msPerSamp 
    scoreCode = tablesCode ++ printf "i1 0.1 %f\n" dur
    dur = msPerSamp * fromIntegral tableLen / 1000

    