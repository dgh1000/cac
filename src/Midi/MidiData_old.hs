{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DefaultSignatures #-}
module Midi.MidiData where


import GHC.Generics
import Control.DeepSeq
import Sound.PortMidi
import Common.CommonData

----------------------------------------------------------------------
----------------------------------------------------------------------
--         convenient midi status and controller values defined here
mkNoteOn :: Int -> Int -> Int -> Int -> RawMidiEvent
mkNoteOn stream chan pitch vel = RawMidiEvent stream chan 0x90 pitch vel

mkNoteOff :: Int -> Int -> Int -> Int -> RawMidiEvent
mkNoteOff stream chan pitch vel = RawMidiEvent stream chan 0x80 pitch vel

mkProgram :: Int -> Int -> Int -> RawMidiEvent
mkProgram stream chan program = RawMidiEvent stream chan 0xC0 program 0

mkCtrl :: Int -> Int -> Int -> Int -> RawMidiEvent
mkCtrl stream chan ctrl value = RawMidiEvent stream chan 0xB0 ctrl value

mkBreath :: Int -> Int -> Int -> RawMidiEvent
mkBreath stream chan value = mkCtrl stream chan 2 value

mkMod :: Int -> Int -> Int -> RawMidiEvent
mkMod stream chan value =  mkCtrl stream chan 1 value

mkVol :: MidiDest -> Int -> RawMidiEvent
mkVol (stream,chan) value = mkCtrl stream chan 7 value

mkExpr :: MidiDest -> Int -> RawMidiEvent
mkExpr (stream,chan) value = mkCtrl stream chan 11 value

mkPan :: Int -> Int -> Int -> RawMidiEvent
mkPan stream chan value = mkCtrl stream chan 10 value





----------------------------------------------------------------------
----------------------------------------------------------------------
-- midi time model (when changing also see MidiUtil)
----------------------------------------------------------------------
----------------------------------------------------------------------

{-
-- Integer is number of milliseconds
data MidiTime = MidiTime Integer
                deriving(Show,Eq,Generic,NFData)

instance Ord MidiTime where
  (<=) (MidiTime x) (MidiTime y) = x <= y
  (<)  (MidiTime x) (MidiTime y) = x <  y
  (>)  (MidiTime x) (MidiTime y) = x >  y

(mtSub) (MidiTime t1) (MidiTime t2) = MidiTime (t1 - t2)
(mtAdd) (MidiTime t1) (MidiTime t2) = MidiTime (t1 + t2)

-- In this model, midi time is expressed in ms.
-- If we change the rounding method here, then we must update
--  midiTimeQuantum below
toMidiTime f = MidiTime $ round (1000 * f)
fromMidiTime :: MidiTime -> Double
fromMidiTime (MidiTime t) = fromIntegral t / 1000

-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--     END midi time model
----------------------------------------------------------------------
----------------------------------------------------------------------

 
----------------------------------------------------------------------
----------------------------------------------------------------------

--               OLD OLD OLD


data RawMidiEventType = NoteOnType
                      | NoteOffType
                      | NoteModifierType
                      | ContinuousType
                        deriving(Show,Eq)


data RawMidiEvent = RawMidiEvent 
  { rmeStream  :: Int
  , rmeChan    :: Int 
  , rmeStatus  :: Int 
  , rmeData1   :: Int
  , rmeData2   :: Int
  }
  deriving (Show,Eq,Generic,NFData)


----------------------------------------------------------------------
----------------------------------------------------------------------

--                    NEW NEW NEW


-- status, data1, and data2
data SingleEvent = SingleEvent Int Int Int


data RelativeTime = RtOn  Double
                  | RtOff Double


data Modif = Modif
  { mcT      :: RelativeTime
  , mcDest   :: Maybe (Int,Int)
  , mcEvt    :: Either SingleEvent Int  -- either some kind of single MIDI
                                        -- control event, or the pitch of a
                                        -- keyswitch
  }


data NoteData = NoteData
  { ndTBeg  :: Double
  , ndTEnd  :: Double
  , ndPitch :: Int
  , ndVel   :: Int
  }


data NoteGroup = NsChord [NoteData]
               | NsTrillTrem [NoteData]


data TimeModel = RegularModel
                 { tmNomTBeg   :: Double
                 , tmNomTEnd   :: Double
                 , tmActTBeg   :: Double
                 , tmActTEnd   :: Double
                 , tmStacDur   :: Maybe Double
                 , tmArpOffset :: Maybe Double
                 }
               | TrillTremModel 
                 { tmNomTBeg :: Double
                 , tmNomTEnd :: Double
                 }
               deriving(Show)


data ChordEvent = ChordEvent
  { ceDest      :: (Int,Int)
  -- , ceTimeModel :: TimeModel
  , ceNotes     :: NoteGroup
  , ceModifs    :: [NoteModif]
  }


data TrillTremEvent = TrillTremEvent
 { teDest      :: (Int,Int)
 , teTBeg      :: Double             -- first note begin
 , teTEnd      :: Double             -- last note end
 , teTimes     :: [(Double,Double)]  -- start and end time of each note in the
                                     -- trill
 , teVel       :: [Int]              -- velocity of each note in the trill
 , tePitches1  :: [Int]
 , tePitches2  :: [Int]
 , teModifs    :: [Modif]
 }


-- okay we want to represent either note that came from a chord, or a trill or
-- tremolo



data MidiEvent =
  NoteEvent      { meDest      :: (Int,Int)
                 , meTimes     :: Times
                 , mePitch     :: Int
                 , meVel       :: Int
                 , meModifs    :: [Modif]
                 }
  |
  TrillTremEvent { meDest      :: (Int,Int)
                 , meTimes     :: Times
                 , meTimesList :: [Times]
                 , mePitches1  :: [Int]
                 , mePitches2  :: [Int]
                 , meModifs    :: [Modif]
                 }
  
  
----------------------------------------------------------------------
----------------------------------------------------------------------

--            OLD OLD OLD continued

data MidiEvent = 
  NoteEvent
    { meTime          :: Integer
    , meTimeOff       :: Integer
    , meOn            :: RawMidiEvent
    , meOff           :: RawMidiEvent
    , meOnMods        :: [RawMidiEvent]
    , meOffMods       :: [RawMidiEvent]
    , meLegatoFlag    :: Bool   -- True if this note should have a legato
                                -- connection to following note
    , meTimeHistory   :: [(Integer,Integer)]
    , meStaff         :: String
    , meSustained     :: Bool
    , meShort         :: Bool
    }
  | SingleEvent 
    { meTime          :: Integer
    , meIsTrackStart  :: Bool
    , meEvt           :: RawMidiEvent
    , meStaffName     :: String
    }
  deriving (Show,Eq,Generic,NFData)


data PrettyMidiEvent = PrettyMidiEvent MidiEvent


isTrackStartEvent e@NoteEvent{}          = False
isTrackStartEvent (SingleEvent _ flag _ _) = flag


meSetEnd e@NoteEvent{meTime=t1, meTimeOff=t2, meTimeHistory=h} tNew = 
  e{meTimeOff=tNew, meTimeHistory= h ++ [(t1,t2)]}


meSetBeg e@NoteEvent{meTime=t1, meTimeOff=t2, meTimeHistory=h} tNew =
  e{meTime=tNew, meTimeHistory=h ++ [(t1,t2)]}
meSetBeg (SingleEvent t    trackStart raw name) tNew = 
         (SingleEvent tNew trackStart raw name)

{-
evtSubTime :: MidiEvent -> Double -> MidiEvent
evtSubTime ne@(NoteEvent evtT evtTOff _ _ _ _ _ _ ) t =
  ne {meTime = evtT-t, meTimeOff = evtTOff-t}
evtSubTime (SingleEvent evtT flag evt staff) t = 
  SingleEvent (evtT-t) flag evt staff
-}

meShift delta e@NoteEvent{meTime=t1, meTimeOff=t2, meTimeHistory=h} =
  e {meTime=t1+delta, meTimeOff = t2+delta, meTimeHistory= h++[(t1,t2)]}


-- Each time a change in times happens, the current times are appended
-- to the history. Therefore times get newer and newer as you move toward
-- the end of the list. The current times (t1,t2) are the latest times.
-- They are not in the list. We put them at the end of the list so that
-- head on empty 'h' case will pick up current times.
meOrigTimeOff NoteEvent {meTime=t1,meTimeOff=t2,meTimeHistory=h} = 
  snd . head $ h ++ [(t1,t2)]


transposeMidi :: Int -> MidiEvent -> MidiEvent
transposeMidi t e@NoteEvent {meOn = onEvt, meOff = offEvt} =
  e {meOn = transposeRawMidi t onEvt, meOff = transposeRawMidi t offEvt}


transposeRawMidi :: Int -> RawMidiEvent -> RawMidiEvent
transposeRawMidi t (RawMidiEvent stream chan status pitch vel) =
  RawMidiEvent stream chan status (pitch+t) vel



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

