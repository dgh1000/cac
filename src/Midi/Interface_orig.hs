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

  preStartDelay: -- Delay caused by moving absolute start time into the past, in order
     -- to handle midi events with negative start times
    (note July 2012: not sure this is really needed any more)





-}

module Midi.Interface 
  ( mkRawEvents
  , playRawEvents
  , realTimeMidiRun
  , allNotesOffAllChans ) where

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
  


----------------------------------------------------------------------
----------------------------------------------------------------------
-- close all streams


----------------------------------------------------------------------
----------------------------------------------------------------------
--                  mkRawEvents -- convert a bunch of MidiEvent's
--                      to [(MidiTime,RawMidiEvent)]



      
-- mkRawEvents
--
-- Given a list of MidiEvent (in which a note-type MidiEvent will be
-- several raw midi events) translate to a list of RawMidiEvent
-- 
-- Will SORT the list.
-- 
-- Will normalize so that events that should be at the start of the
-- track will
-- be played at time 0, and everything else will be normalized to
-- start at 'initialEvtsDelay'. SingleEvent-type midi events have a field
-- which is a flag indicating they should go at the start of the track.
-- NoteEvent-type midi events have no flag like this.
--
-- Also this will shift "note modifiers" into the past 
-- by 'modifierEarlyBy' to allow any 
-- controllers/keyswitches to take effect before it is played.
--
-- Return: sorted list of (time,event) pairs
-- 
mkRawEvents :: Integer -> [MidiEvent] -> [(MidiTime, RawMidiEvent)]
mkRawEvents initialEvtsDelay evts 
  = sortNormalizeEvents initialEvtsDelay raws
  where
    {-
    xMidiEvent (NoteEvent t on off timeOff mods) =
      [(t, on), (timeOff, off)] ++ map g mods
      where
        g e = (t `mtAdd` MidiTime (-modifierEarlyBy), e)
    xMidiEvent (SingleEvent t evt) = [(t, evt)]
    raws = concatMap xMidiEvent evts
    -}
    raws :: [(MidiTime,Bool,RawMidiEvent)] -- (<time>,<supposed to be start
                                           --  of track?>, <event>)
    raws = concatMap xMidiEvent evts
    xMidiEvent (SingleEvent t isTrackStart rme) = [(t,isTrackStart,rme)]
    xMidiEvent (NoteEvent t on off timeOff mods _) =
      [(t,False,on), (timeOff,False,off)] ++ concatMap g mods
      where
        g e 
          | isNoteOn e = 
            [ (t `mtAdd` MidiTime (- MPD.modifierEarlyBy),False,e)
            , (t `mtAdd` MidiTime (- MPD.modifierOffEarlyBy), False, 
                   convertOnToOff e)]
          | otherwise = [(t `mtAdd` MidiTime (- MPD.modifierEarlyBy),False,e)]
          where
            isNoteOn (RawMidiEvent _ _ x _ _) = x .&. 0xf0 == 0x90
            convertOnToOff (RawMidiEvent str ch st dat1 dat2) =
              RawMidiEvent str ch (st .&. 0xef) dat1 dat2


-- sortNormalizeEvents
--   sort and normalize events.
-- Will normalize so that events marked to be at start of track will
-- be played at time 0, and everything else will be normalized to
-- start at 'initialEvtsDelay'
--
--  Inputs
--    MidiTime: initial evts delay in milliseconds
--    
sortNormalizeEvents :: Integer -> [(MidiTime, Bool,RawMidiEvent)] -> 
                       [(MidiTime, RawMidiEvent)]
sortNormalizeEvents initialEvtsDelay evts = final
  where
    (startEvts,regularEvts) = L.partition (\(_,flag,_) -> flag) evts
    sortedRegEvts = sortBy (\(t1,_,_) (t2,_,_) -> compare t1 t2) regularEvts
    tMin = case sortedRegEvts of
     [] -> throwMine "in sortNormalizeEvents: no regular midi events found."
     (t,_,_):_ -> t
    tOffset = tMin `mtSub` (MidiTime initialEvtsDelay)
    xRegEvt (t1,_,e) = (t1 `mtSub` tOffset, e)
    xStartEvt (_,_,e) = (MidiTime 0, e)
    final = if tMin `mtSub` tOffset <= MidiTime 1
      then throwMine "fdl9kj432"
      else map xStartEvt startEvts ++ map xRegEvt sortedRegEvts
    

----------------------------------------------------------------------
----------------------------------------------------------------------
--          playRawEvents -- do the actual playing in real time

-- playRawEvents
--   Real-time playback of midi data in the input form 
--     [(MidiTime,RawMidiEvent)] 
--      
playRawEvents :: [PMStream] -> CULong -> [(MidiTime, RawMidiEvent)] -> IO ()
playRawEvents _ _ [] = do
  putStrLn "\nDone." 
  return ()
playRawEvents streams absBegin ((MidiTime t, rme): remain) = do
  spinUntil $ fromIntegral absBegin + t
  let streamId = rmeStream rme
  when (streamId >= length streams) 
       (throwMine $ "In playRawEvents, got stream id greater than number of"++
                    " MIDI streams available")
  writeShort (streams !! streamId) $ toPMEvent rme
  playRawEvents streams absBegin remain

spinUntil :: Integer -> IO ()
spinUntil t = do
  threadDelay 500
  c <- time
  if fromIntegral c < t then spinUntil t else return ()

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
  where msg = PMMsg (fromIntegral $ chan + status - 1) 
              (fromIntegral data1) (fromIntegral data2)
