module App.MidiPlayer.MidiPlayerData where

import Data.Map(Map)
import Sound.PortMidi

.. don't need this module...

data PlayArg = PlayArgMsrRange MsrRange
             | PlayArgDumpMusDoc
             | PlayArgDumpPerfDoc
             | PlayArgCsdOnly String  -- csd filename
             | PlayArgQuit
             | PlayArgTerminateProcess
             | PlayArgCwd Int
             | PlayArgTempoRatio Float
               deriving(Eq)


-- Represents an arg like 8 or 8-10
type MsrRange = (Int, Maybe Int)

-- Expressing result of parsing the input line
data InpCmd  = InpQuit
             | InpPlay [MsrRange] 
             | InpDumpMusDoc
             | InpDumpPerfDoc
             | InpParseError String
             | InpTerminateProcess
             | InpCwd Int          -- change working directory to
                                   -- one of hard-coded directories

-- we are using the definition of AllConfig in CsoundPlayer
{-
data AllConfig = AllConfig
  { timeDiv :: Int
  , blankMsrs :: Int }
-}