
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DefaultSignatures,
             TypeSynonymInstances, FlexibleInstances #-}
module Translation.TranslationData where


import qualified Data.Map as M
import Text.Parsec hiding(State)
import GHC.Generics
import Control.DeepSeq
import System.Random
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Identity
import Data.Map(Map)
import Data.Maybe
import Data.Set(Set)
import Common.CommonData
import Score.ScoreData
import Midi.MidiExport
import Util.Exception


----------------------------------------------------------------------
{-

"patterns" in time and dynamics

  how should they be computed and represented?

    Translation.hs


-}


----------------------------------------------------------------------


configModifierEarlyBy = 20 :: Integer  -- milliseconds
configModifierOffBy   = 10 :: Integer  -- milliseconds


----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
--                   CONFIG FILE STATEMENT REPRESENTATIONS


----------------------------------------------------------------------
--               top level, ConfigStatement

-- This represents statements which appear in the playback config file
data ParsedConfSt = PcsStaff StaffConfig
                  | PcsStaffIgnore String
                  | PcsSusPed Double Double
                  | PcsComment
                  | PcsPattern PatternSt
                  | PcsTimingVariation TimingVariation


----------------------------------------------------------------------
--                 Pattern Statement


data ParsedSubPat = PspSubMeas PatternSubMeas
                  | PspSubDiv  PatternSubDiv


data ParsedPatList = PplDyn [Double]
                   | PplTempo [Double]


data PatternSt = PatternSt
  { pstName    :: String
  , pstSubMeas :: Map (Int,Int) PatternSubMeas
  , pstSubDiv  :: Map Int       PatternSubDiv
  }


data PatternSubMeas = PatternSubMeas
  { psmTimeSig  :: (Int,Int)
  , psmNSubMeas :: Int
  , psmNSubDiv  :: Int
  , psmDynTempo :: [(Double,Double)]   -- (<dyn>,<tempo>)
  }


data PatternSubDiv = PatternSubDiv 
  { psdN        :: Int 
  , psdDynTempo :: [(Double,Double)] 
  }



----------------------------------------------------------------------
--                   staff statement


-- All information that appears in a 'staff' entry in the config file.
data StaffConfig = StaffConfig
  { stcName      :: String
  , stcInstrName :: String
  , stcMute      :: MuteState
  , stcMidiDests :: Map String (Int,Int)
  , stcParams    :: Map String ConfigValue
  , stcSusPed    :: SusPedUse
  }



data SusPedUse = SusPedIgnore | SusPedSource | SusPedApply
               deriving(Eq)

data MuteState = MuteNone | MuteAll | MuteControllers | MuteNotes


{-

  staff statements

    fields literally in staff statement

      staff name

      instr name

      mute state (whether this staff is muted)

      midi dests (midi local channel name, [channel names are needed to
      distinguish between multiple channels needed for some single instruments
      such as strings in QLSO], mapped to midi stream # and chan #)

      params:  things like velocity random variation

      sus ped use: indicates if this staff is to be the sus ped source, is to
      have sus ped applied, or is to ignore sus ped

    consolidated data from all staff sts. in useful form

      staff name, instr name, mute, midi dests, params

        in short, everything but sustain pedal

      sustain pedal info needs to be grouped into what is now called
      SusPedConfig: timing info, and naming source staff and application
      staves

    so staff statements are pretty useful in the form they are parsed, with
    only one piece of information that will be redundant to SusPedConfig

  susped statement

    this is combined with staff sus ped flags to make SusPedConfig

  pattern statement

    


-}

----------------------------------------------------------------------



----------------------------------------------------------------------
--              Sustain Pedal Configuration

-- <lift start delta> <lift duration> <source staff> <application staves>
data SusPedConfig = SusPedConfig Double Double String [String]




--              END CONFIG FILE DATA
----------------------------------------------------------------------
----------------------------------------------------------------------


-- 
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
                         
data FullDebugInfo = FullDebugInfo
  { fdiTimeMap :: Map String (AbsTimeMap,Map Int LoudnessFunc,[LoudnessDebug])
  , fdiUnitTimeMods :: [UnitTimeMod]
  }


data WrappedMidiEvent = WrappedMidiEvent MidiEvent



data PitchRecord = TrillTremPitchRecord [Int] [Int]
                 | SinglePitchRecord    Int
                   deriving(Show)



----------------------------------------------------------------------
----------------------------------------------------------------------
--             express patterns in terms of divisions into
--               submeasures and sub-submeasures

----------------------------------------------------------------------
----------------------------------------------------------------------
--                    truncate/extend configuration for
--                       a given instrument

data TruncExtConfig = TruncExtConfig
  { tecMinDur       :: Integer
  , tecMinSep       :: Integer
  , tecSustainedSep :: Maybe Integer
  , tecExtend       :: Maybe Integer
  , tecLegExtend    :: Maybe Integer
  , tecMaxRatio     :: Double
  }


----------------------------------------------------------------------
----------------------------------------------------------------------
--                      events record

data EventsRecord = 
   NoteEventsRecord   {erStaffName :: String, erEvts :: [MidiEvent]}
 | ModEventsRecord    {erStaffName :: String, erEvts :: [MidiEvent]}
 | FixedEventsRecord  {erStaffName :: String, erEvts :: [MidiEvent]}
 | SusPedEventsRecord {erStaffName :: String, erEvts :: [MidiEvent]}
 deriving(Generic,NFData)


isNoteEventsRecord (NoteEventsRecord _ _) = True
isNoteEventsRecord _                      = False

----------------------------------------------------------------------
----------------------------------------------------------------------
--       Data representing translation-specific concepts


----------------------------------------------------------------------
----------------------------------------------------------------------
--                 random functions


----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
--                    warp

-- (warp location) (maybe left loc) (maybe right loc) amount direction
data Warp2 = Warp2 Loc (Maybe Loc) (Maybe Loc) Double Double


----------------------------------------------------------------------
----------------------------------------------------------------------
--             MetaInstr definitions


data QTechnique = QtArco
                | QtPizz
                | QtExpr

{-

data Generic = Generic
  { gDest            :: (Int,Int)
  , gArp             :: Bool
  , gShortenStaccato :: Bool
  , gLegato          :: Maybe Double
  , gSepSame         :: Maybe Double
  , gLegatoModifier  :: Int
  , gVelRange        :: (Int,Int)
  , gVelDelta        :: Int
  }

-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                       parsing


data ConfigEntry = CEMetaInstr Elem
                 | CETimingVariation

-- if we want to parse any number of things which can be in any order, they
-- must be same data type. okay how about they don't have to be in any order?
-- fixed order?


{-
instance ConfigValueClass String where
  toValue (ConfigValueString s) = Just s
  toValue _                     = Nothing


instance ConfigValueClass Int where
  toValue (ConfigValueInt i) = Just i
  toValue _                  = Nothing


instance ConfigValueClass Double where
  toValue (ConfigValueDouble d) = Just d
  toValue _                     = Nothing
-}

-- so we need some way to get "maybe a" where a could be a single value. can
-- this be done with same class in
