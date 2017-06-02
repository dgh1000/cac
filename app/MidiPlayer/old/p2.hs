
import System.IO
import qualified App.MidiPlayer.RunFunctions as MR
-- import qualified App.CsoundPlayer.RunFunctions as CR


main = loop

loop = do
  putStr "Enter q, m<port number>, or c:"
  hFlush stdout
  li <- getLine
  case li of
    'm' : rs -> 
       case reads rs of
         [] -> do { putStrLn "Something wrong in port number." ; loop  }
         (i,_):_ -> do { MR.topLevel i ; loop } -- XXXXX
    -- "c" -> do { CR.topLevelLoop ; loop }
    "q" -> return ()
    _ -> loop
