import System.IO
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Sound.PortMidi


int :: Parser Int
int = many digit >>= return . read


cmd :: Parser (Int,Int,Int)
cmd = do
  many space
  i1 <- int
  many1 space
  i2 <- int
  many1 space
  i3 <- int
  return (i1,i2,i3)
  

showMidiDevice :: Int -> IO ()
showMidiDevice x = do
  di <- getDeviceInfo x
  putStrLn $ printf "%d: %s '%s'" x (decideInputOrOutput di) (name di) 


decideInputOrOutput di = if input di then "Input :" else "Output:"


main = do
  c <- countDevices
  mapM_ showMidiDevice [0..c-1]
  putStr "Enter midi port number, channel, control number sep by spaces:"
  hFlush stdout
  li <- getLine
  case parse cmd "" li of
    Left err -> putStrLn $ show err
    Right (port,chan,cc) -> sendControl port chan cc

  
sendControl :: Int -> Int -> Int -> IO ()
sendControl deviceNum chan cc = do
  initialize
  dev <- openOutput deviceNum 0
  case dev of
    Right err -> putStrLn $ show err
    Left stream -> do
      let m = PMMsg (fromIntegral $ chan-1+0xB0) (fromIntegral cc) 0
      writeShort stream $ PMEvent (encodeMsg m) 0
      close stream
      terminate
      return ()
      


  
  
  
