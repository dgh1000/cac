-- Play midi data through PortMidi.

import Data.Ratio
import Control.Concurrent
import Debug.Trace
import Text.XML.MusicXML 
import Text.XML.MusicXML.Partwise 
import Sound.PortMidi
import Util.FileUtil
import Util.Showable
import Util.Env
import Midi.Converters.FromMusDoc
import MusDoc.Data
import MusDoc.FromXml
import MusDoc.Processing
import MusDoc.ShowMusDoc
import Debug.Trace

-- Assumes evts are sorted.
-- putAtCurrentTime :: Timestamp -> [PMEvent] -> Int -> ([PMEvent], Timestamp)
putAtCurrentTime curTime evts beginBuf = (map g evts, timestamp $ last evts)
  where g (PMEvent m ts) = PMEvent m (ts+curTime-delta+fromIntegral beginBuf)
        delta = timestamp $ head evts

go :: PMStream -> [PMEvent] -> Int -> Int -> IO PMError
go stream evts beginBuf dieAway = do 
  ts <- time
  let (theEvts,dur) = putAtCurrentTime ts evts beginBuf
  err <- writeEvents stream theEvts
  print err
  threadDelay $ fromIntegral $ 1000*(dur+fromIntegral dieAway)
  close stream

mainTmp = do
  let beginLoc = Loc 1 (1%1)
      endLoc = Loc 2 (1%1)
  file <- mostRecentFile "c:/Mike/music/algo/compositions/finale/2011" "xml"
  putStrLn $ "File read is: " ++ file
  xmlDoc' <- read_FILE read_MusicXML_Partwise file
  let xmlDoc = fromOK xmlDoc'
      musDoc = toMusDoc xmlDoc
      filtered = filterDoc (\loc _ -> beginLoc <= loc && loc <= endLoc) musDoc
  writeFile "out.txt" $ (showItem . showi) filtered
  


main = do
  
  -- Configuration
  port <- defaultMidiDevice
  let {-port = 10  -- Midi Yoke Output 1 -}
      searchDir = "c:/Mike/music/algo/compositions/finale/2011"
      beginBuffer = 100  -- milliseconds
      dieAway = 1000     -- milliseconds
      beginLoc = Loc 1 (1%1)
      endLoc = Loc 2 (1%1)
  
  initialize
  res <- openOutput port 1000 
  case res of 
    Left stream -> do
      file <- mostRecentFile searchDir "xml"
      xmlDoc' <- read_FILE read_MusicXML_Partwise file
      let xmlDoc = fromOK xmlDoc'
          -- filterDoc :: (Loc -> Note -> Bool) -> Part -> Part
          evts = toMidi 
                 . filterDoc (\loc _ -> beginLoc <= loc && loc <= endLoc)
                 . toMusDoc 
                 $ xmlDoc
      if length evts > 0
        then do err <- go stream evts beginBuffer dieAway
                putStrLn $ "Result from closing: " ++ show err
        else putStrLn "\nNo events to play (maybe because all were filtered)\n"
    Right err -> print err
  terminate
