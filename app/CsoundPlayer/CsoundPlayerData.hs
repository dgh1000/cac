module App.CsoundPlayer.CsoundPlayerData where

import Data.Map(Map)
import PlaybackPI.PlaybackPIData( InstrMap
                            , Mixer 
                            , InstrPlay(..) )
import Common.CommonData( Loc )

{-

data ConfigStatement = ConfigInstrPlay String InstrPlay
                     | ConfigMixer Mixer
                     | ConfigTimeDivison Int
                     | ConfigBlankMsrs Int
                     | ConfigComment
                     | ConfigCommand Command
                       deriving(Show)



data Command = CommandTimewarp String String Float Float Float Rational
             | CommandExaggerateTime (Map Rational Float)
             | CommandRampTowardBeat1 Float Float
             | CommandMeasureTimeRamp Float Float

               deriving(Show)
-}

data RunMode = RealtimeMode
             | FileMode

----------------------------------------------------------------------
--                     command-line args data
----------------------------------------------------------------------

{-
data PlayCom = PQuit
             | Play [PlayArg]
-}

{-
data PlayArg = PlayArg PlayArgType Loc
data PlayArgType = PlayArgBegin
                 | PlayArgEnd
-}

{-

      --- MOVED THIS TO Playback module ----------

-- Represents an arg like 8 or 8-10
type MsrRange = (Int, Maybe Int)

data PlayArg = PlayArgMsrRange MsrRange
             | PlayArgDumpMusDoc
             | PlayArgDumpPerfDoc
             | PlayArgCsdOnly String  -- csd filename
             | PlayArgQuit
             | PlayArgTerminateProcess
             | PlayArgCwd Int
             | PlayArgTempoRatio Float
               deriving(Eq)


-- Expressing result of parsing the input line
data InpCmd  = InpQuit
             | InpPlay [MsrRange] (Maybe String) (Maybe Float)
                -- Bool is a flag to write csd only
                -- Maybe Float is possible alteration to tempo
             | InpDumpMusDoc
             | InpDumpPerfDoc
             | InpParseError String
             | InpTerminateProcess
             | InpCwd Int          -- change working directory to
                                   -- one of hard-coded directories


-}
