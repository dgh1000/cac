

import Util.RandomState
import Util.MonadStacks

countSegments :: Float -> Float -> [Float] -> [Int]
countSegments low high values = map g pairs
  where
    boundaries = [low,low+1..high]
    pairs = zip boundaries (tail boundaries)
    g (x,y) = length (filter (\z -> z > x && z < y) values)

{-
doErrorRand :: ErrorRand [Int]
doErrorRand = do
  rs <- rdGausses 
  let working = take 10000 rs
  let count x y = (length (filter (\z -> z > x && z < y) working)) :: Int
  let d1 = count (-4) (-3.5)
  let d2 = count (-3.5) (-3)
  let d3 = count (-3) (-2.5)
  let d4 = count (-2.5) (-2)
  let d5 = count (-2) (-1.5)
  let d6 = count (-1.5) (-1)
  let d7 = count (-1) (-0.5)
  let d8 = count 0 0.5
  let d9 = count 0.5 1
  let d10 = count 1 1.5
  let d11 = count 1.5 2
  let d12 = count 2 2.5
  let d13 = count 2.5 3
  let d14 = count 3 3.5
  let d15 = count 3.5 4
  return [d8,d9,d10,d11,d12,d13,d14,d15]
-}

doErrorRand :: ErrorRand [Int]
doErrorRand = do
  rs <- rdGaussesScale 10 20
  return $ countSegments (10) 20 (take 10000 rs)

main = do
  rd <- freshRandData
  case evalER doErrorRand rd of
    Left err -> putStrLn err
    Right v -> print v

