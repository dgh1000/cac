
main = do
  writeFile "data.txt" $ concatMap (\x -> ", " ++ show x) $ take 90 values

values :: [Float]
values =  map (\x -> 1/x) [1..]