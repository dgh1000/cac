
import System.IO
import qualified App.MidiPlayer.RunFunctions as MR

main = loop

loop = do
  putStr "Enter port number:"
  hFlush stdout
  li <- getLine
  case reads li of
    [] -> do { putStrLn "Something wrong in port number." ; loop }
    (i,_):_ -> do { MR.topLevel i (i+1)}


