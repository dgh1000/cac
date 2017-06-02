
computeTableSize :: Int -> Int
-- computeTableSize nOutSamples = 2 ^ (ceiling $ 2 ** logBase 2 nOutSamples)
computeTableSize nOutSamples = 
 2 ^ (ceiling $ logBase 2 (fromIntegral nOutSamples))

main = do
  print $ computeTableSize 257