
import Data.Array.Unboxed
import Text.Printf
import Translation.Mod 
import Translation.TranslationData
import Util.Math
import Util.Showable
import Midi.ShowMidi


testMkLoudArray = (mkLoudArray loudFn n idxToTime,idxToTime)
  where
    loudFn x | x <= 2    = 2
             | otherwise = 1
    tBeg = 1
    tEnd = 3
    spacing = 0.05
    (n,idxToTime) = computeIndices spacing tBeg tEnd

showArray = 
  concatMap (\(i,x) -> printf "%3d %8.2f\n" i x) .
  assocs

main = do
  let (a,_) = testMkLoudArray
  putStrLn . showArray . smoothArray 3 $ a

----------------------------------------------------------------------

valueFn x = [ round $ scale 1 x 2 0 127
            , round $ scale 1 x 2 50 60 ]

main2 = do
  let config = ModConfig (0,1) [0,11] valueFn 0.05 0.003
      (a,idxToTime) = testMkLoudArray
  writeFile "out.txt" . 
    showItem .
    Component "" False .
    map showi . 
    concatMap (\(idx,l) -> makeModMidi config (idxToTime idx) l) .
    assocs .
    smoothArray 3 $ a
