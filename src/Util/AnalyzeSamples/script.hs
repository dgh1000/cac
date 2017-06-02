
import Control.Exception

foo :: IO Bool
foo = do
  (

main2 :: String -> IO ()
main2 s = do
  result <- try (readFile "thing.dat") 
  case result of
    Left e -> do
      putStrLn $ "error reading file" ++ show (e :: IOException)
      writeFile "thing.dat" s
    Right data_ -> 
      putStrLn $ "Got: " ++ data_