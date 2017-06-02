
import qualified Data.Map as M
import Text.Printf
import Control.Monad.Trans
import Instruments.InstrumentsData
import Translation.TranslationData
import Instruments.Run
import Instruments.Piano
import Util.Math
import Common.CommonData


dests = M.fromList [ ("Keyboard-staff1", (0,1))
                   , ("Keyboard-staff2", (0,2)) ]

metas = Meta "piano1" (M.keys dests) (pianoInit dests) pianoRun

tVar = TimingVariation 3 7 1.0 1.0 (-0.3) 0.3

main2 s = putStrLn s

main = run $ RunData [metas] tVar
