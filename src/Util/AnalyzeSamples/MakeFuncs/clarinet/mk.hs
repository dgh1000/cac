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
gen10DataFile = "/Mike/Music/algo/haskell/Csound/Instruments/configurations/gen10.bin"
pvsDataRoot = "/Temp/csound/tables/clarinet"
maxFreq = 10000
partialWidth = 0.25  -- call this W. when analyzing the energy in a 
                 -- partial N with fundamental F, include any pvs analysis bin
                 -- within the frequency range (N*F - W/2, N*F + W/2)



----------------------------------------------------------------------
----------------------------------------------------------------------
-- sub-functions
----------------------------------------------------------------------
----------------------------------------------------------------------
updateConfigFile :: Int -> OnePitch -> IO ()
updateConfigFile midiPitch op = do
  b <- B.readFile gen10DataFile 
  let blazy = BL.pack . B.unpack $ b
      d = decode blazy
      d2 = M.insert midiPitch op d
      b2 = encode d2
  BL.writeFile gen10DataFile b2


----------------------------------------------------------------------
----------------------------------------------------------------------
-- main functions
----------------------------------------------------------------------
----------------------------------------------------------------------

mkDefaultConfig = do
  let wa = WaveformAttack [] []
      wr = WaveformRelease [] []
      op = OnePitch wa wr wa wa wr
      allPitches = M.fromList [(0,op)] :: Map Int OnePitch
      b = encode (Gen10Data allPitches)
  BL.writeFile gen10DataFile b




mk60 = do 
  {- let soundFile =  "/giga/vienna/18-Clarinet-Bb/chopped/KLB_pA_sus_mp_C4.wav"
      pvsDataSubPath = "/leg-mp/"
      midiPitch = 60
      dur = 1.5
      timeWidthT1 = 0.010
      timeWidth = 0.015
      t0 = 0.070   -- where sound starts
      t1 = 0.110   -- where spectrum 0 is sampled
      t5 = 0.700   -- where attack ends and steady-state starts
      t6 = 1.050   -- where steady-state ends and release begins
      t7 = 1.300
      t8 = 1.400
      sourceDur = 1.650  -- where input file goes silent
      whichChannel = PvsLeftChOnly
  -}  
  let soundFile =  "/giga/vienna/18-Clarinet-Bb/chopped/KLB_pA_sus_mp_C4.wav"
      pvsDataSubPath = "/leg-mp/"
      midiPitch = 60
      dur = 1.5
      timeWidthT1 = 0.010
      timeWidth = 0.015
      t0 = 0.070   -- where sound starts
      t1 = 0.110   -- where spectrum 0 is sampled
      t5 = 0.700   -- where attack ends and steady-state starts
      t5 = 0.950   -- where attack ends and steady-state starts
      t6 = 1.050   -- where steady-state ends and release begins
      t7 = 1.100
      t8 = 1.150
      sourceDur = 1.250  -- where input file goes silent
      whichChannel = PvsLeftChOnly

      pvsDataDir = pvsDataRoot ++ pvsDataSubPath ++ show midiPitch ++ "/"
  status <- pvsanal midiPitch pvsDataDir soundFile  whichChannel dur
  
  putStrLn "\n\nNow reading PvsDump and processing it."
  pvsData <- readPvsDump pvsDataDir
  let (t2,t3,t4,tab0,tab1,tab2,tab3,tab4,tab5,tab6) =
        mkTables353 t1 t5 t7 t8 timeWidthT1 timeWidth maxFreq pvsData
      [pbt1,pbt2,pbt3,pbt4,pbt5] = [t1-t0,t2-t0,t3-t0,t4-t0,t5-t0]
      att = WaveformAttack [pbt1,pbt2,pbt3,pbt4,pbt5]
            [tab0,tab1,tab2,tab3,tab4]
      rel = WaveformRelease [t6-sourceDur,t7-sourceDur,t8-sourceDur]
           [tab5,tab6]
      op = OnePitch att rel att att rel
  updateConfigFile 60 op
  