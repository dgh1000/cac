module Midi.MidiData where

import Sound.PortMidi

{-

Discussion of adding random timing offsets to MIDI data. 

Goals of algorithm:

   - offset note beginnings by various algorithms which specify maximum
     offset in absolute sense and also max offset as fraction of note length

   - offset note endings to provide "legato" effect randomly

   - avoid "clashing": duration of a note overlapping with a second note in
     same channel of same pitch

   - have the ability to offset notes that are simultaneous in the source
     score by different amounts

Simplest thing is to first offset beginnings while making sure no
overlaps. Then extend endings while making sure no overlaps. Or extend
endings and beginnings without checking, then check endings against
beginnings and trim endings or beginnings if necessary. 




-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--         convenient midi status and controller values defined here
mkNoteOn :: Int -> Int -> Int -> RawMidiEvent
mkNoteOn chan pitch vel = RawMidiEvent chan 0x90 pitch vel

mkNoteOff :: Int -> Int -> Int -> RawMidiEvent
mkNoteOff chan pitch vel = RawMidiEvent chan 0x80 pitch vel

mkProgram :: Int -> Int -> RawMidiEvent
mkProgram chan program = RawMidiEvent chan 0xC0 program 0

mkCtrl :: Int -> Int -> Int -> RawMidiEvent
mkCtrl chan ctrl value = RawMidiEvent chan 0xB0 ctrl value

mkBreath :: Int -> Int -> RawMidiEvent
mkBreath chan value = mkCtrl chan 2 value

mkMod :: Int -> Int -> RawMidiEvent
mkMod chan value =  mkCtrl chan 1 value

mkVol :: Int -> Int -> RawMidiEvent
mkVol chan value = mkCtrl chan 7 value

mkPan :: Int -> Int -> RawMidiEvent
mkPan chan value = mkCtrl chan 10 value





----------------------------------------------------------------------
----------------------------------------------------------------------
-- midi time model (when changing also see MidiUtil)
----------------------------------------------------------------------
----------------------------------------------------------------------


-- Integer is number of milliseconds
data MidiTime = MidiTime Integer
                deriving(Show,Eq)

instance Ord MidiTime where
  (<=) (MidiTime x) (MidiTime y) = x <= y
  (<) (MidiTime x) (MidiTime y) = x < y
  (>) (MidiTime x) (MidiTime y) = x > y

(mtSub) (MidiTime t1) (MidiTime t2) = MidiTime (t1 - t2)
(mtAdd) (MidiTime t1) (MidiTime t2) = MidiTime (t1 + t2)

-- In this model, midi time is expressed in ms.
-- If we change the rounding method here, then we must update
--  midiTimeQuantum below
toMidiTime f = MidiTime $ round (1000 * f)



----------------------------------------------------------------------
----------------------------------------------------------------------
--     END midi time model
----------------------------------------------------------------------
----------------------------------------------------------------------

 
data RawMidiEventType = NoteOnType
                      | NoteOffType
                      | NoteModifierType
                      | ContinuousType
                        deriving(Show,Eq)


data RawMidiEvent = RawMidiEvent 
  { rmeChan :: Int 
  , rmeStatus :: Int 
  , rmeData1 :: Int
  , rmeData2 :: Int
  }
  deriving (Show,Eq)


data MidiEvent = 
  NoteEvent
    { meTime :: MidiTime
    , neOn :: RawMidiEvent
    , neOff :: RawMidiEvent
    , neTimeOff :: MidiTime
    , neNoteModifiers :: [RawMidiEvent]
    , neLegatoFlag :: Bool   -- True if this note should have a legato
                             -- connection to following note 
    }
  | SingleEvent 
    { meTime :: MidiTime
    , seIsTrackStart :: Bool
    , ceEvt :: RawMidiEvent
    }
  deriving (Show,Eq)

type MidiEventsWithDest = (MidiDest,[MidiEvent])
type MidiChan = Int
type MidiStreamId = Int
type MidiDest = (MidiStreamId,MidiChan)

instance Ord MidiEvent where
  compare e1 e2 = compare (meTime e1) (meTime e2)

-- Any application running real-time midi needds to have some code doing
-- initialization of a midi port, and closing the midi port afterward. 
-- I wanted to encapsulate this code in
-- the Interface.hs module, in the function realTimeMidiRun.
--
-- Most midi applications will have a loop
-- that takes commands and executes them and repeats until user quits.
--
-- realTimeMidiLoop needs to call the application-specific loop, and
-- the application-specific loop will need to have some context that is
-- an argument in order to preserve state from one iteration to the next.
-- The thing that realTimeMidiLoop needs to do is create the initialized
-- context to be used as the argument on the first call to the loop.
-- the context must be in a class that provides such a function.
class MidiLoopContext a where
  mkInitMidiLoopContext :: PMStream -> a

{-

XXX

class MidiLoopContext a where
  mkInitMidiLoopContext :: [PMStream] -> a

-}
