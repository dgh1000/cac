import System.Random

{-
choosePhase :: Gen Integer
choosePhase = do
  x <- choose (0, 359)
  return x
-}




main = do
  gen <- newStdGen
  let ns = randomRs (1,5) gen :: [Int]
  print $ take 10 ns

main2 = do
  gen <- newStdGen
  let (gen1, gen2) = split gen
      ns = randomRs (1,5) gen1 :: [Int]
      fs = randoms gen2 :: [Float]
  print $ zip (take 10 ns) (take 10 fs) 

-- exponential series
main3 = do
  let s = iterate (* 0.75) 1
  putStrLn $ unlines $ take 10 (map show s)


partials :: IO [(Float,Int)]
partials = do
  gen <- newStdGen
  let strengths = iterate (* 0.5) 1 :: [Float]
      phases = randomRs (0,359) gen :: [Int]
  return $ zip strengths phases

main4 = do
  ps <- partials
  putStrLn $ unlines $ map show (take 10 ps)
  

{-

import System.Random

main = do
   gen <- newStdGen
   
     a StdGen is an instance of RandomGen, it's an "object" with 
       the genRange, next and split functions. this is what one
       passes around

   let ns = randoms gen :: [Int]

       randoms :: StdGen -> [a]       
       randomRs :: (a,a) -> StdGen -> [a]

   print $ take 10 ns



randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)

    mkStdGen is used instead of newStdGen
        (newStdGen is used in IO monad, gets seed from???
         newStdGen needs a seed

main :: IO ()
main = do print $ take 10 (randomList 42 :: [Float])

   42 is the seed


import Data.List ( sortBy )
import Data.Ord ( comparing )
import System.Random ( Random, RandomGen, randoms, newStdGen )

main :: IO ()
main =
 do gen <- newStdGen
    interact $ unlines . unsort gen . lines

unsort :: (RandomGen g) => g -> [x] -> [x]
unsort g es = map snd . sortBy (comparing fst) $ zip rs es
  where rs = randoms g :: [Integer]

  - comparing has signature   (a -> b) -> b -> b -> Ordering

  - g is gen


-}

