
import Data.IORef


main :: IO ()
main = do
  ir <- newIORef 1
  loop ir
       
loop :: IORef Int -> IO ()
loop ir = do
  i <- readIORef ir
  putStrLn $ "Currently: " ++ show i
  s <- getLine
  writeIORef ir (read s)
  loop ir

