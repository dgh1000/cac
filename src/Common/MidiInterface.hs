
module Common.MidiInterface where

import qualified Sound.PortMidi as SP
import qualified Data.List as L
import Text.Printf
import Sound.PortMidi hiding(name,initialize)
import Data.Either
import Data.Function
import Control.Monad
import Control.Concurrent
import Common.CommonData
import Util.Exception


startMidi :: Int -> Int -> IO (Either PMError [PMStream])
startMidi midiDevNumLow midiDevNumHigh = do
  SP.initialize
  result <- mapM openOutputDevice [midiDevNumLow..midiDevNumHigh]
  let (errors,streams) = partitionEithers result
  case errors of
    [] -> return $ Right streams
    (err:_) -> return $ Left err


stopMidi :: [PMStream] -> IO ()    
stopMidi streams = do
  mapM_ close streams
  terminate
  return ()    


openOutputDevice :: Int -> IO (Either PMError PMStream)
openOutputDevice devNum = do
  result <- openOutput devNum 0
  case result of
    Left stream -> return $ Right stream
    Right err -> return $ Left err


-- playRawEvents
--   Real-time playback of midi data in the input form 
--     [(MidiTime,RawMidiEvent)] 
--      
-- 
playRawEvents :: [PMStream] -> Integer -> [MidiData] -> IO ()
playRawEvents streams absBegin shorts = do
  let f :: MidiData -> [(Integer,(Int,Int,Int,Int))]
      f (MdNote t1 t2 str chan pit vel) =
        [ (round $ 1000*t1,(str, 0x80+chan-1, pit, vel))
        , (round $ 1000*t2,(str, 0x90+chan-1, pit, vel)) ]
      f (MdCtrl t1 str chan data1 data2 ) =
        [ (round $ 1000*t1,(str, 0xb0+chan-1, data1, data2)) ]
      tuples = L.sortBy (compare `on` fst) $ concatMap f shorts
  putStrLn $ printf "last MIDI event: %.1f secs"
             ((fromIntegral . fst $ last tuples)/1000::Double)
  playRawEvents' streams absBegin tuples


playRawEvents' _ _ [] = putStrLn "\nDone." >> return ()
playRawEvents' streams absBegin ((t,(streamId,x,y,z)):remain) = do
  spinUntil $ absBegin+t
  when (streamId >= length streams) 
    (throwMine $ "In playRawEvents', got stream id greater than number " ++
       "of MIDI streams available")
  writeShort (streams !! streamId) $ toPMEvent (x,y,z)
  playRawEvents' streams absBegin remain


-- toPMEvent
--   Converts RawMidiEvent to PMEvent (note, the latter includes a time,
--   but since we do playback with the PortMidi's function writeShort, 
--   the PortMidi library will ignore the time, so we just set it to zero)
toPMEvent :: (Int,Int,Int) -> PMEvent
toPMEvent (status,data1,data2) = PMEvent msg 0
  where msg = encodeMsg $ PMMsg (fromIntegral status) (fromIntegral data1)
                                (fromIntegral data2)

-- spinUntil: takes number of milliseconds
spinUntil :: Integer -> IO ()
spinUntil t = do
  threadDelay 500
  c <- time
  if fromIntegral c < t then spinUntil t else return ()


