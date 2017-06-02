
import qualified Data.List as L
import Text.Printf
import Data.Ratio
import System.Random
import Util.Math


x = print $ [1%1, 1 + num % nDivs..1 + num%1]
  where
    num   = 6 :: Integer
    nDivs = 4 :: Integer

{-
thing gen = takeWhile pred xs
  where
    g :: Double -> Int -> (Double,Int)
    g x y = (x+fromIntegral y,y)
    rs :: [Int]
    rs = randomRs (3,5) gen
    xs = L.mapAccumL g 0 rs
    pred :: (Double,[Int]) -> Bool
    pred (x,_) = x < 20
-}


takeUntilSum :: Int -> Int -> [Int] -> [Int] -> [Int]
takeUntilSum goal currentSum source current
  | currentSum >= goal = current
  | otherwise = takeUntilSum goal (currentSum+x) (drop 1 source) (x:current)
  where
    x = head source


main = do
  gen <- newStdGen
  let xs = randomRs (3,5) gen
  print $ takeUntilSum 20 0 xs []

xx = (9 :: Double) * 0.050 * 30 * 0.16

test10 = do
  let whenXBig = 0.01
      whenXSmallRatio = 0.1
      g (ratio,fixed) x = printf "%.4f" y
        where
          x2 = x*ratio
          y :: Double
          y | x2 > fixed = fixed
            | otherwise  = x2
      xs = [0.001,0.01,0.03,0.05,0.09,0.1,0.2,0.5,1.0]
      ratio = 0.1
      fixed = 0.05
  mapM_ (\x -> putStrLn $ g (ratio,fixed) x) xs
  




{-



we want something that as x goes to infintity, y goes to fixed value A

as x goes to zero, y goes to B*x



-}
