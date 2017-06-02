----------------------------------------------------------------------
----------------------------------------------------------------------
-- for working with Gen10ImitateSamplerV2
----------------------------------------------------------------------
----------------------------------------------------------------------

import Debug.Trace
import Text.Printf
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Map(Map)
import Util.Math
import Csound.Instruments.Gen10
import Util.CLib.PvsanalInterface
import Util.CLib.CLibData( PvsAnalysisChannel(..)
                         , PvsDump(..) )
import Util.AnalyzeSamples.Lib

-- config
gen10DataFile = "/Mike/Music/algo/haskell/Csound/Instruments/configurations/gen10Violin.bin"
pvsDataRoot = "/Temp/csound/tables/violin"
maxFreq = 15000
partialWidth = 0.25  -- call this W. when analyzing the energy in a 
                 -- partial N with fundamental F, include any pvs analysis bin
                 -- within the frequency range (N*F - W/2, N*F + W/2)



----------------------------------------------------------------------
----------------------------------------------------------------------
-- sub-functions
----------------------------------------------------------------------
----------------------------------------------------------------------
{-
updateConfigFile :: Int -> OnePitch -> IO ()
updateConfigFile midiPitch op = do
  b <- B.readFile gen10DataFile 
  let blazy = BL.pack . B.unpack $ b
      d = decode blazy
      d2 = M.insert midiPitch op d
      b2 = encode d2
  BL.writeFile gen10DataFile b2
-}


----------------------------------------------------------------------
----------------------------------------------------------------------
-- main functions
----------------------------------------------------------------------
----------------------------------------------------------------------

{-
mkDefaultConfig = do
  let wa = WaveformAttack [] []
      wr = WaveformRelease [] []
      op = OnePitch wa wr wa wa wr
      allPitches = M.fromList [(0,op)] :: Map Int OnePitch
      b = encode (Gen10Data allPitches)
  BL.writeFile gen10DataFile b
-}

rebuild = rebuildGen10Config gen10DataFile


doAttack fileNameSuffix t0 t1 t5 timeWidthT1 timeWidth partialWidth 
  midiPitch pvsData = do
  let (t2,t3,t4,s0,s1,s2,s3,s4) = mkAttSpecs353 t1 t5 timeWidthT1 timeWidth 
        maxFreq partialWidth pvsData
      att = WaveformAttack [t1-t0,t2-t0,t3-t0,t4-t0,t5-t0]
            [s0,s1,s2,s3,s4]
      fna = printf "m%02d_%s.spc" midiPitch fileNameSuffix
  B.writeFile fna (B.pack . BL.unpack . encode $ att)
  putStrLn $ "Done writing " ++ fna
  return s4

doRelease fileNameSuffix t6 t7 t8 sourceDur timeWidth partialWidth midiPitch 
  pvsData = do
  let (s4,s5,s6) = mkRelSpecs353 t6 t7 t8 timeWidth maxFreq partialWidth 
                   pvsData
      rel = WaveformRelease [t6-sourceDur,t7-sourceDur,t8-sourceDur] [s4,s5,s6]
      fnr = printf "m%02d_%s.spc" midiPitch fileNameSuffix
  B.writeFile fnr (B.pack . BL.unpack . encode $ rel)
  putStrLn $ "Done writing " ++ fnr
  return s4

-- makes slow attack from silence, and slow release into silence, and
-- steady-state
mk60_sas_srs_ss = do 
  let soundFile =  "/giga/vienna/05-Solo-violin/chopped/VI_mV_sus_mp_C4.wav"
      pvsDataSubPath = "/leg-mp/"
      midiPitch = 60
      timeWidthT1 = 0.010
      timeWidth = 0.015
      t0 = 0.010   -- where sound starts
      t1 = 0.040   -- where spectrum 0 is sampled
      t5 = 0.160   -- where attack ends and steady-state starts
      t6 = 1.600   -- where steady-state ends and release begins
      t7 = 1.800
      t8 = 2.000
      sourceDur = 2.250  -- where input file goes silent
      whichChannel = PvsAverageChs
      partialWidth = 0.25

      pvsDataDir = pvsDataRoot ++ pvsDataSubPath ++ show midiPitch ++ "/"
  status<- pvsanal midiPitch pvsDataDir soundFile  whichChannel (sourceDur+0.2)
  putStrLn "\n\nNow reading PvsDump and processing it."
  pvsData <- readPvsDump pvsDataDir

  steadyStateA <- 
    doAttack "sas" t0 t1 t5 timeWidthT1 timeWidth partialWidth midiPitch 
             pvsData
  steadyStateB <-
    doRelease "srs" t6 t7 t8 sourceDur timeWidth partialWidth midiPitch pvsData

  let steadyStateAvg = M.unionWith (\x y -> (x+y)/2) steadyStateA steadyStateB
  let fns = printf "m%02d_ss.spc" midiPitch
  BL.writeFile fns (encode steadyStateAvg)
  putStrLn $ "Done writing " ++ fns

mk60_fas = do 
  let soundFile="/giga/vienna/05-Solo-violin/chopped/VI_0aT1-legsus_pp_C4.wav"
      pvsDataSubPath = "/0aT1-mp/"
      midiPitch = 60
      timeWidthT1 = 0.010
      timeWidth = 0.015
      t0 = 0.010   -- where sound starts
      t1 = 0.020   -- where spectrum 0 is sampled
      t5 = 0.120   -- where attack ends and steady-state starts
      sourceDur = 0.150 -- where input file goes silent
      whichChannel = PvsAverageChs
      paritalWidth = 0.25

      pvsDataDir = pvsDataRoot ++ pvsDataSubPath ++ show midiPitch ++ "/"
  status<-pvsanal midiPitch pvsDataDir soundFile  whichChannel (sourceDur+0.2)
  putStrLn "\n\nNow reading PvsDump and processing it."
  pvsData <- readPvsDump pvsDataDir

  doAttack "fas" t0 t1 t5 timeWidthT1 timeWidth partialWidth midiPitch pvsData
  return ()


