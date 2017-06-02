{-

Example of how to use this module. Note: mkRawEvents and playRawEvents
have to be separate calls because we want any exceptions thrown by
mkRawEvents to be caught before forking a new thread to call playRawEvents

do
      rmes <- evaluate $ mkRawEvents MPID.modifierEarlyBy 
              MPID.initialEventsDelay midiEvts 
      let (MidiTime tEnd,_) = last rmes
      putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Float)
      beginTime <- time
      tid <- forkIO $ 
           playRawEvents stream (beginTime + fromIntegral preStartDelay) rmes

where

  preStartDelay: -- Delay caused by moving absolute start time into the past,
                 -- in order to handle midi events with negative start times
    (note July 2012: not sure this is really needed any more)



-}

module Midi.Interface  where

import Data.Function
import Control.Concurrent
import Control.Monad
import Data.Bits
import Foreign.C.Types
import Sound.PortMidi
import Midi.MidiData
import qualified MidiPI.MidiPIData as MPD
import Data.List( sortBy )
import qualified Data.List as L
import Util.Exception


{-

----------------------------------------------------------------------
----------------------------------------------------------------------
--  top level loop -- handles opening and closing stream
--
--  
--   Inputs
--     (a -> IO ()) - the application function loop
realTimeMidiRun :: MidiLoopContext a => (a -> IO ()) -> [Int] -> IO ()
realTimeMidiRun appFnLoop deviceNums = do
  initialize
  result <- openOutput deviceNum 0
  case result of
    Left stream -> do
      appFnLoop (mkInitMidiLoopContext stream)
      allNotesOffAllChans stream
      close stream
      terminate
      return ()
    Right err -> do
      putStrLn $ "Error opening midi device.\n" ++ show err



-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--  top level loop -- handles opening and closing stream
--
--  
--   Inputs
--     (a -> IO ()) - the application function loop
realTimeMidiRun :: MidiLoopContext a => (a -> IO ()) -> Int -> IO ()
realTimeMidiRun appFnLoop deviceNum = do
  initialize
  result <- openOutput deviceNum 0
  case result of
    Left stream -> do
      appFnLoop (mkInitMidiLoopContext stream)
      allNotesOffAllChans stream
      close stream
      terminate
      return ()
    Right err -> do
      putStrLn $ "Error opening midi device.\n" ++ show err



----------------------------------------------------------------------
----------------------------------------------------------------------
--           notes off utilities

allNotesOffAllChans :: PMStream -> IO ()
allNotesOffAllChans stream =
  mapM_ (allNotesOff stream) [1..16]


allNotesOff stream chan = writeShort stream $
        toPMEvent (RawMidiEvent 0 chan 0xb0 123 0)
  

pedalOffAllChans :: PMStream -> IO ()
pedalOffAllChans stream = 
  mapM_ (pedalOff stream) [1..16]

pedalOff stream chan = writeShort stream $
        toPMEvent (RawMidiEvent 0 chan 0xb0 0x40 0) 

----------------------------------------------------------------------
----------------------------------------------------------------------
-- close all streams


----------------------------------------------------------------------
----------------------------------------------------------------------
--                  mkRawEvents -- convert a bunch of MidiEvents
--                      to [(MidiTime,RawMidiEvent)]


{-

-- mkRawEvents
--
--   Convert map of MidiEvent into sorted list of RawMidiEvent.
--
--   Ouput tuples are (<time>,<event>), with the times adjusted as follows:
--
--     1. track start events will be paired with time 0
--     2. other events will be shifted in time so that first one is at 
--        'initDelay'
--
--   'initDelay' is in milliseconds
mkRawEvents :: Integer -> [MidiEvent] -> [(Integer,RawMidiEvent)]
mkRawEvents initDelay evts =
   -- 'meShift' will modify time on/off of all events by same amount. 
   -- But track start events will then have their time set to 0 by 
   -- mkRawEventsOne
   sortBy (compare `on` fst) .
   concatMap (mkRawEventsOne . meShift shiftAmt) $ evts
  where
   shiftAmt = initDelay-tMin
   tMin = minimum . map meTime . filter (not . isTrackStart) $ evts
 

mkRawEventsOne :: MidiEvent -> [(Integer,RawMidiEvent)]
mkRawEventsOne (NoteEvent t tOff on off mods _ _ _ _) =
  [(t,on),(tOff,off)] ++ concatMap (oneMod t) mods
mkRawEventsOne (SingleEvent t isTrackStart raw _) | isTrackStart = [(0,raw)]
                                                  | otherwise    = [(t,raw)]


oneMod :: Integer -> RawMidiEvent -> [(Integer,RawMidiEvent)]
oneMod t e | isNoteOn e = [ (t-configModifierEarlyBy,e)
                          , (t-configModifierOffBy, convertOnToOff e) ]
           | otherwise  = [ (t-configModifierEarlyBy,e) ]


isNoteOn (RawMidiEvent _ _ x _ _) = x .&. 0xf0 == 0x90


convertOnToOff (RawMidiEvent str ch st dat1 dat2) =
  RawMidiEvent str ch (st .&. 0xef) dat1 dat2


isTrackStart (SingleEvent _ flag _ _) = flag
isTrackStart _                        = False
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--          playRawEvents -- stream RawMidiEvent to midi port
--                          in real time


-- playRawEvents
--   Real-time playback of midi data in the input form 
--     [(MidiTime,RawMidiEvent)] 
--      
-- 
playRawEvents :: [PMStream] -> Integer -> [(Integer, RawMidiEvent)] -> IO ()
playRawEvents _ _ [] = do
  putStrLn "\nDone." 
  return ()
playRawEvents streams absBegin ((t,rme):remain) = do
  spinUntil $ absBegin + t
  let streamId = rmeStream rme
  when (streamId >= length streams) 
       (throwMine $ "In playRawEvents, got stream id greater than number of"++
                    " MIDI streams available")
  writeShort (streams !! streamId) $ toPMEvent rme
  playRawEvents streams absBegin remain

configDurTimeClick = 0.001


-- spinUntil: takes number of milliseconds
spinUntil :: Integer -> IO ()
spinUntil t = do
  threadDelay 500
  c <- time
  if fromIntegral c < t then spinUntil t else return ()


{-
mergeLists :: Ord k => k -> [[(k,a)]] -> [(k,a)]
mergeLists prevMin ls
  | null fs = []
  | otherwise = 
      let m = let x = minimum . map fst $ fs
              in if x <= prevMin
                   then throwMine "error in Interface.hs" 
                   else x
          nexts = concatMap (takeWhile ((==m) . fst)) ls
          remains = map (dropWhile ((==m) . fst)) ls
      in nexts ++ mergeLists m remains
  where
    fs = concatMap (take 1) $ ls
-}

{-
mergeLists :: Ord k => [[(k,a)]] -> [(k,a)]
mergeLists ls
  | null fs = []
  | otherwise = 
      let m = minimum . map fst $ fs
          nexts = concatMap (takeWhile ((==m) . fst)) ls
          remains = map (dropWhile ((==m) . fst)) ls
      in nexts ++ mergeLists m remains
  where
    fs = concatMap (take 1) $ ls
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--       utility to convert to the actual type used by the PortMidi
--       library, PMEvent

-- toPMEvent
--   Converts RawMidiEvent to PMEvent (note, the latter includes a time,
--   but since we do playback with the PortMidi's function writeShort, 
--   the PortMidi library will ignore the time, so we just set it to zero)
toPMEvent :: RawMidiEvent -> PMEvent
toPMEvent (RawMidiEvent _ chan status data1 data2) = PMEvent msg 0
  where msg = encodeMsg $ PMMsg (fromIntegral $ chan + status - 1) 
              (fromIntegral data1) (fromIntegral data2)
