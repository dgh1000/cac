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
import Common.CommonExport
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


configSlicesPerBeat   = 16  :: Int
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

data MiStatement = MiStatement
  { misType :: String
  , misName :: String
  , misStaves :: [String]
  , misParams :: [(String,ConfigValue)]
  }



----------------------------------------------------------------------
--              Sustain Pedal Configuration

-- <lift start delta> <lift duration> <source staff> <application staves>
data SusPedConfig = SusPedConfig Double Double String [String]



----------------------------------------------------------------------

-- Timing variation is executed as follows:
--
--   Come up with the set of all Locs that notes occur at, in any part, called
--   L. Pair up each Loc in L with its successor. That makes a bunch of pairs
--   P. For each pair, stretch or shrink time within that interval. 
--
--   To determine how much to stretch or shrink time, 
--
-- minLen and maxLen specify the range of lengths of each subsequence. Any
-- given subsequence length is chosen pseudorandomly to be between these,
-- inclusive.
--
-- For each subsequence, a list ratio_1, ratio_2, etc. is formed from
-- arithmetic sequence from tvRatio1 to tvRatio2. And delta_1, delta_2,
-- etc. is formed from tvDelta1 to tvDelta2.
-- 
-- ratio_i indicates the amount an interval will change as a fraction of its
-- length. if the delta change computed by this ratio is greater than than
-- delta_i, then delta_i is chosen instead.
data TimingVariation = TimingVariation 
  { tvMinLen   :: Int   
  , tvMaxLen   :: Int
  , tvRatio1   :: Double
  , tvRatio2   :: Double
  , tvDelta1   :: Double
  , tvDelta2   :: Double
  }


----------------------------------------------------------------------
--            Putting all playback configuration together

-- name for type of statement that indicates an expressive pattern to impose
-- on a named section of the composition expression statement

data ExprStatement = ExprStatement


data ConfigFile = ConfigFile
  { cfMiSts   :: Map String MiStatement
  , cfExprSts :: [ExprStatement]
  , cfTVar    :: Maybe TimingVariation
  }



--              END CONFIG FILE DATA
----------------------------------------------------------------------
----------------------------------------------------------------------


-- 
data RelTimeMap = RelTimeMap (Map Loc Double)

data AbsTimeMap = AbsTimeMap (Map Loc Double)

----------------------------------------------------------------------
----------------------------------------------------------------------
--  data needed for translation: dynamics and loudness functions

type LoudnessFunc = Either Loc Double -> Double

data DynSeg a = DynSeg Double Double a
            deriving(Show)

-- A loudness curve is made of segments. The begin Loc of each segment is
-- its key in the Map. The begin and end dynamic levels, and the end Loc,
-- are in the data type DynSeg. 
--
-- Sucessive segments may or may not be contiguous. That is, if two keys
-- k1 and k2 appear in the map where k2 is the next key greater than k1, 
-- then the end Loc of the segment at k1 may be equal to k2 or may be less
-- than k2. If it is less than k2, this implies the presence of a hole.
-- If we perform a map lookup at loc X, and X is in one of the holes (or
-- before the first segment or after the last segment) then the computed
-- result will be 0.
type LoudnessCurve a = Map a (DynSeg a)


data LoudnessDebug = LoudnessDebug
  { ldName  :: String
  , ldCurve :: LoudnessCurve Loc
  }


type StaffLoudnessFunc = Map Int LoudnessFunc


----------------------------------------------------------------------
----------------------------------------------------------------------
data PlayCmd = PlayCmd { pcmMsrRange :: (Int,Maybe Int)
                       , pcmTempoRatio :: Double
                       , pcmTranspose :: Int
                       }


----------------------------------------------------------------------
----------------------------------------------------------------------
                         
-- A StateT and WriterT monad that is used during translation from Score to
-- MIDI events. It contains the Score and several items computed once from
-- the Score but used over and over, such as time maps and loudness
-- curves. It also contains the Instruments and playback configuration.
-- 

-- type Tr a = StateT TrState (Writer [OutputItem]) a
type Tr a = State TrState a




-- PlaybackConfig holds parsed config file stuff, command line options. no
-- reason to make it separate. maybe config file stuff separate. this is one
-- reason to separate parsed parameters for meta instrument... we can then
-- have a playback config for just config file, and m.i.'s can go in TrState.


data TrState = TrState
  -- input configuration
  { tsConfigFile :: ConfigFile
  , tsPlayCmd    :: PlayCmd
  , tsScore      :: Score
                    
  -- random generator                   
  , tsRandomGen  :: StdGen

  -- computed state used in producing output
  , tsAbsTimeMaps :: Map String AbsTimeMap
  , tsLoudFuncs  :: Map String StaffLoudnessFunc
  , tsMsrRange   :: (Int,Int)

  -- meta-instruments, which hold both initial parameters and state that is
  -- folded while translating
  , tsMis        :: Map String MetaInstr

  -- converted note output
  , tsOutput          :: [[MidiEvent]]
  }


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

data PatternData = PatternData (Map Loc (Double,Double))  
  -- (<dynamic alteration>, <tempo ratio>)


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


-- Any given Note has three translation scenarios.
data TranslationCase = SingleCase Loc
                     | TrillCase Int
                     | TremoloCase
                       deriving(Show)



----------------------------------------------------------------------
----------------------------------------------------------------------
--                 random functions

trRandomR :: (Random a) => (a,a) -> Tr a
trRandomR (lo,hi) = do
  gen <- gets tsRandomGen
  let (output,gen') = randomR (lo,hi) gen
  modify (\s -> s {tsRandomGen = gen'})
  return output


trRandomRs :: (Random a) => (a,a) -> Tr [a]
trRandomRs (lo,hi) = do
  gen <- gets tsRandomGen
  let (g1,g2) = split gen
  modify (\s -> s {tsRandomGen = g2})
  return $ randomRs (lo,hi) g1

----------------------------------------------------------------------
----------------------------------------------------------------------
          --                  tempo ramps, pauses


-- UnitWarp with the Either field as Left:
--
--    the first Loc is at the warp mark.
--
-- UnitAbsWarp:; the first Loc is at the warp mark
data UnitTimeMod = UnitRamp  Loc Loc Double Double
                 | UnitPause Loc Rational
                 | UnitWarp (Maybe String) (Either (Loc,Loc) (Loc,Loc,Loc))
                            Rational
                 | UnitAbsWarp Loc Loc Rational


isRamp (UnitRamp _ _ _ _)           = True
isRamp _                            = False

isPause (UnitPause _ _)             = True
isPause _                           = False

isWarpLocal (UnitWarp m _ _)        = isJust m
isWarpLocal _                       = False

isWarpGlob (UnitWarp m _ _)         = isNothing m
isWarpGlob _                        = False

isAbsWarp (UnitAbsWarp _ _ _)       = True
isAbsWarp _                         = False


----------------------------------------------------------------------
----------------------------------------------------------------------
--                    warp

-- (warp location) (maybe left loc) (maybe right loc) amount direction
data Warp2 = Warp2 Loc (Maybe Loc) (Maybe Loc) Double Double


----------------------------------------------------------------------
----------------------------------------------------------------------
--             MetaInstr definitions


data MetaInstr =
  MiPiano { miName          :: String
          , miStaffNames    :: [String]
          , miParams        :: [Elem]
          , miTranslateCk   :: MetaInstr -> ChordKey -> Tr ()
          , miTranslateMark :: MetaInstr -> String -> Loc -> Mark -> Tr ()
          , miStacDur       :: Map String Double
          , miArpDelta      :: Double
          , miTrillShape    :: Map String TrillShape
          , miTremShape     :: Map String TrillShape
          , pnoDests        :: Map String (Int,Int)
          }
  

----------------------------------------------------------------------
----------------------------------------------------------------------
--                       parsing


-- if we want to parse any number of things which can be in any order, they
-- must be same data type. okay how about they don't have to be in any order?
-- fixed order?


data Elem = Bracketed Int Int String [Elem]
          | Param Int Int String ConfigValue
          | Single Int Int ConfigValue


type Exc = Except String


runExc :: Exc a -> a
runExc m = case runIdentity (runExceptT m) of
  Left msg -> throwMine msg
  Right v -> v


{-

data Elem = EBracketed String [Elem]
          | ESingle String Value
          | EInstr String
          | EStaves [String]
          | EMeta String [String] [Elem] 
          deriving(Show)


data MetaInstrP = MetaInstrP String [String] (Map String ConfigValue)
                deriving(Show)

-}

{-
class ConfigValueClass a where
  toValue :: ConfigValue -> Maybe a
-}


class ElemValueClass a where
  toValue :: Elem -> Maybe a


instance ElemValueClass Double where
  toValue (Single _ _ (ConfigValueDouble d)) = Just d
  toValue (Single _ _ _) = Nothing
  toValue (Param _ _ _ (ConfigValueDouble d)) = Just d
  toValue (Param _ _ _ _) = Nothing
  toValue (Bracketed _ _ _ _) = Nothing
  

instance ElemValueClass String where
  toValue (Single _ _ (ConfigValueString d)) = Just d
  toValue (Single _ _ _) = Nothing
  toValue (Param _ _ _ (ConfigValueString d)) = Just d
  toValue (Param _ _ _ _) = Nothing
  toValue (Bracketed _ _ _ _) = Nothing
  

instance ElemValueClass Int where
  toValue (Single _ _ (ConfigValueInt d)) = Just d
  toValue (Single _ _ _) = Nothing
  toValue (Param _ _ _ (ConfigValueInt d)) = Just d
  toValue (Param _ _ _ _) = Nothing
  toValue (Bracketed _ _ _ _) = Nothing
  

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
