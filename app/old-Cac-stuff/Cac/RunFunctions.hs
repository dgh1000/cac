{-# LANGUAGE TypeSynonymInstances #-}

module App.Cac.RunFunctions where

import System.IO
import System.Directory
import Text.Parsec
import Text.Parsec.String
import Sound.PortMidi
import Control.Concurrent
import Control.Exception
import Prelude hiding (catch)
import Midi.MidiExport
import Midi.Interface
import PlaybackCac.ParseCmdLine
import PlaybackCac.PlaybackCacExport
import Cac.Core.CoreExport
import Cac.Core.ToMidi
import Util.Exception

data LoopContext = LoopContext
  { lcStream ::PMStream
  , lcTid :: Maybe ThreadId }

instance MidiLoopContext LoopContext where
    mkInitMidiLoopContext stream = LoopContext stream Nothing

cacPlay :: Node -> IO ()
cacPlay node = do
  realTimeMidiRun (loop  node) 13
 
{-
cacPlay :: [MidiEvent] -> IO ()
cacPlay mes = realTimeMidiRun (loop mes) 13
-}

loop :: Node -> LoopContext -> IO ()
loop node lc@(LoopContext stream tid) = do
  parsedCmd <- lineFetch `catches` allHandlersM
  case parsedCmd of
    Nothing -> loop node lc
    Just InpCmdStop ->
      case tid of
        Nothing -> loop node lc
        Just tidx -> do
          killThread tidx
          allNotesOffAllChans stream
          loop node lc
    Just InpCmdQuit -> return ()
    Just (InpCmdPlay (PlayParams mBeg mEnd)) -> do
     let mes = toMidi node mBeg mEnd
     case tid of 
       Nothing -> return ()
       Just x -> do
         killThread x
     allNotesOffAllChans stream
     tid <- go mes stream
     loop node lc { lcTid = tid }

{-
nodeToMidiEvts :: Node -> PlayParams -> [MidiEvent]
nodeToMidiEvts node = 
  -- have to flatten in a sense, easy to find labels or traverse find markers
-}
  


lineFetch :: IO (Maybe InpCmd)
lineFetch = do
  d <- getCurrentDirectory
  putStrLn d
  putStr "(midi)---> "
  hFlush stdout
  li <- getLine
  if length li == 0
    then lineFetch
    else do  
      r <- evaluate $ lineParse li
      return (Just r)

lineParse ::  String -> InpCmd
lineParse s =
  case parse parseCmdLine "" s of 
    Left err -> InpParseError (show err)
    Right cmd -> cmd


go :: [MidiEvent] -> PMStream -> IO (Maybe ThreadId)
go mes stream = do
  {-
  -- config
  let deltaT = 0.25
      firstNote = 30
      nNotes = 30
  let nes = map mkNoteEvent [0..nNotes-1]
      mkNoteEvent noteNum = NoteEvent tOn on off tOff []
        where
          midiPitch = firstNote + noteNum
          tOnMs = fromIntegral noteNum * deltaT
          tOffMs = tOnMs + deltaT - 0.05     
          tOn = toMidiTime tOnMs
          tOff = toMidiTime tOffMs
          on = mkNoteOn 1 (noteNum + firstNote) 64
          off = mkNoteOff 1 (noteNum + firstNote) 64
  -}
  rmes <- evaluate $ mkRawEvents 6 100 mes
  beginTime <- time
  tid <- forkIO $ playRawEvents stream (beginTime + 200) rmes
  return $ Just tid

{-
class (ExceptionReturn a) b where
    erZero :: b

 
-- type MaybeA a = Maybe a
instance ExceptionReturn Maybe where
  erZero = Nothing

ioHandlerA :: IOException -> IO a
ioHandlerA e = do
  putStrLn (show e)
  return erZero
-}

allHandlersM = [Handler myHandlerM, Handler ioHandlerM]

myHandlerM :: MyException -> IO (Maybe a)
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s
  return Nothing

ioHandlerM :: IOException -> IO (Maybe a)
ioHandlerM e = do
  putStrLn (show e)
  return Nothing



