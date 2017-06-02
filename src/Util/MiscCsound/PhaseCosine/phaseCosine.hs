import System.Random
import Control.Monad
import Control.Arrow

partials :: IO [(Float,Int)]
partials = do
  gen <- newStdGen
  let strengths = iterate (* 0.5) 1 :: [Float]
      phases = randomRs (0,359) gen :: [Int]
  return $ zip strengths phases

showTuple :: (Int,(Float,Int)) -> String
showTuple (n,(str,phs)) = " " ++ 
                             show n ++ " " ++ show str ++ " " ++ show phs
makeTuples :: Int -> IO String
makeTuples nResults = do
  ps <- partials
  let all :: [(Int,(Float,Int))]
      all = zip [1..] ps 
      ss :: [String]
      ss = map showTuple all
  return $ concat (take nResults ss)

makeTuples0 :: Int -> IO String
makeTuples0 nResults = do
  ps <- partials
  let all = zip [1..] (map (second $ const 0) ps)
      ss = map showTuple all
  return $ concat (take nResults ss)

midiKeyToCps :: Int -> Float
midiKeyToCps key =   440 * (2 ** ((fromIntegral key - 69)/12))

numPartialsNeeded :: Int -> Int
numPartialsNeeded key = round (22050 / (midiKeyToCps key)-1)

main = do
  let g :: Int -> IO String
      g n = liftM (prefix ++) $ makeTuples $ numPartialsNeeded n
          where prefix = "f" ++ show n ++ " 0 2049 9 "
  ls <- mapM g [40..90]
  writeFile "tables.sco" $ unlines ls


