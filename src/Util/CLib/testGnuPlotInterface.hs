import Data.Map(Map)
import qualified Data.Map as M
import Util.MiscCsound.CLib.GnuPlotInterface
import Text.Printf

main = do
  let rs = mkRanges 100 15000
  mapM_ (\(x,y) -> putStrLn (printf "%f %f" x y)) rs

t100 = M.fromList [(100,1),(200,1),(210,1),( 12000,1)] 

main2 = do
  let c = thirdOctaveHist 100 t100
  putStrLn (showCurve c)

showCurve :: Map Float Float -> String
showCurve c = unlines . map (\(k,a) -> printf "%12f %12f" k a) $ M.toList c

  