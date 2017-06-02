
import qualified  Instruments.Piano_examples as PE
import Instruments.InstrumentsData
import System.Environment
import Instruments.RunOnce

tVar = TimingVariation 3 7 0.99 1.01 (-0.3) 0.3

main = getArgs 
  >>= runOnce (RunData [PE.typical_1 "Solo" "Synth Brass" 100 90] tVar)
