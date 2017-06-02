
import System.IO
import Control.Monad.IO.Class
import System.Console.Haskeline
import Util.Exception

{-
main :: IO ()
main = runInputT defaultSettings
  (bracket
    (liftIO $ )
    (liftIO . hClose)
    (liftIO . myCode $ fp))
-}


-- main = runInputT defaultSettings (liftIO loop) `catches` allHandlersM


main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      s <- getInputLine ":"
      b <- liftIO $ loopIO s
      case b of
        Just False -> return ()
        Just True  -> loop


loopIO :: Maybe String -> IO (Maybe Bool)
loopIO l = do
  case l of
    Just "quit" -> return $ Just True
    Just _      -> return $ Just False





myCode :: Handle -> IO ()
myCode fp = do
  b <- hGetContents fp
  putStrLn b


  
allHandlersM = [Handler myHandlerM, Handler ioHandlerM]


  
myHandlerM :: MyException -> IO (Maybe a)
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s
  return Nothing


ioHandlerM :: IOException -> IO (Maybe a)
ioHandlerM e = do
  putStrLn (show e)
  return Nothing


  




    
