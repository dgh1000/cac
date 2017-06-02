import Control.Monad
import Util.RandomState
import Util.UtilData


ter = do
  let ws = [5,4,1]
  ans <- replicateM 100000 (weightedChoice ws)
  let c x = length $ filter (==x) ans
  return (c 0, c 1, c 2)

erStuff :: Cacm Int
erStuff = do
  rs <- replicateM 1000 (rdGaussScale (100::Float) (200 :: Float))
  let count low high = length . filter (\x -> x <= high && x >= low)
  return $ count 100 200 $ rs
  -- return $ take 50 rs

main = do
  rd <- freshRandData
  case runCM erStuff rd of
    ((Left err,_),_) -> putStrLn err 
    ((Right v,_),_) -> print v
