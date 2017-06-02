import Data.Maybe
import System
import System.Directory
import Control.Monad

main = do
  d <- getCurrentDirectory
  print d
  setCurrentDirectory "old"
  e <- getCurrentDirectory
  print e

computeSnipHoles :: [(Int,Int)] -> Int -> [(Int,Int)]
computeSnipHoles playRanges n = catMaybes $ map processPair pairs
  where
  addendum = [(0,0)] ++ playRanges ++ [(n+1,0)]
  pairs = zip addendum (tail addendum)
  processPair :: ((Int,Int),(Int,Int)) -> Maybe (Int,Int)
  processPair ((_,x),(y,_)) = 
    if (x+1) == y 
    then Nothing
    else Just (x+1,y)

t1 = computeSnipHoles [(3,5)] 10
