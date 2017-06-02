
import Text.Printf
import Util.Math

-- fundamental  ff
-- max freq     m
-- # of harmonics    m `div` ff
--


-- makeAdjSawtoothHarms
--
-- Inputs
--   Float :: fundamental
--   Float :: max frequency
--   Float :: lowest frequency of transition to low amplitude
--   Float :: high frequency of transition to low amplitude
--   Float :: multiplier to get low amplitude
-- Outputs
--   [Float] :: strength of each harmonic
makeAdjSawtoothHarms :: Float -> Float -> Float -> Float -> Float -> [Float]
makeAdjSawtoothHarms fund mfreq lowTran highTran mult = map g [1..nHarms]
  where
    nHarms = round $ mfreq / fund :: Int
    lowTranHarm = round $ lowTran / fund :: Int
    highTranHarm = round $ highTran / fund :: Int
    g :: Int -> Float
    g n 
      | n < lowTranHarm = f
      | lowTranHarm <= n && n <= highTranHarm = 
          f * scale (fromIntegral lowTranHarm) (fromIntegral n)
              (fromIntegral highTranHarm) 1 mult
      | otherwise = f * mult
      where
        f = 1 / fromIntegral n
   

-- makeAdjSawtoothTab
--  Inputs
-- Float: fundamental
-- Float: max frequency
-- Float :: lowest frequency of transition to low amplitude
-- Float :: high frequency of transition to low amplitude
-- Float :: multiplier to get low amplitude
-- Int:   # points in table
-- Int:   table number
makeAdjSawtoothTab :: Float -> Float -> Float -> Float -> Float -> Int ->
                      Int -> String
makeAdjSawtoothTab fund mfreq lowTran highTran mult nPoints num =
  "f" ++ show num ++ " 0 " ++ show nPoints ++ " 10 " 
  ++ concatMap sh harms
  where
    harms = makeAdjSawtoothHarms fund mfreq lowTran highTran mult
    sh h = printf "%.8f " h


test1 = makeAdjSawtoothHarms 1000 10000 5000 5500 0.001
test2 = makeAdjSawtoothHarms 1000 10000 20000 40000 0.001
test3 = do
  mapM_ print (makeAdjSawtoothHarms 500 12000 4000 8000  0.001)
test4 = putStrLn $ makeAdjSawtoothTab 500 15000 4000 8000  0.001 8192 1


    