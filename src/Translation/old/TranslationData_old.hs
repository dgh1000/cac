{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DefaultSignatures #-}
module Translation.TranslationData where


import qualified Data.Map as M
import GHC.Generics
import Control.DeepSeq
import System.Random
import Control.Monad.State
import Control.Monad.Writer
import Data.Map(Map)
import Data.Maybe
import Data.Set(Set)
import Common.CommonExport
import Score.ScoreData
import Midi.MidiExport


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


{-
data PatternEntry = PEDynamics Int [Double] 
                      -- <# subdivisions> [<relative dynamics>], where
                      -- rel. dyns. are specified as 'loudness', that is 1.0
                      -- is one dyanamic step (such as mf to f)
                  | PETempo    Int [Double]
                      -- <# subdivisions> [<tempo ratio>] where a tempo ratio >
                      -- 1.0 means faster, and <1.0 means slower
                  | PETimeSig  Int Int Int
                      -- <numerator> <denominator> <consider this many
                      -- subdivisions in a measure>


-- Consolidation of information in a 'pattern' statement in a more useful form
data PatternDescr = PatternDescr
  { pdDynTempo :: Map Int [(Double,Double)]
  , pdTimeSigs :: [((Int,Int),Int)]
  }
                    deriving(Show)
-}


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

{-
data PlConfFile = PlConfFile
  { pcfStaffSts    :: Map String StaffConfig
  , pcfStaffIgnore :: Set String  -- staff names to ignore in translation
  , pcfSusPed      :: Maybe SusPedConfig
  , pcfPatternSts  :: Map String PatternSt
  , pcfTVar        :: Maybe TimingVariation
  }
-}

-- name for type of statement that indicates an expressive pattern to impose
-- on a named section of the composition expression statement

data ExprStatement = ExprStatement


data ConfigFile = ConfigFile
  { pcfMiSts   :: Map String MiStatement
  , pcfExprSts :: [ExprStatement]
  , pcfTVar    :: Maybe TimingVariation
  }



--              END CONFIG FILE DATA
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------


-- The following is 'playback configuration' ; that is, data which is needed
-- at the time the music is converted from a Score to MIDI in order to make
-- some specific choices about how notes are converted. For instance this
-- includes an indication of which Instrument should be used on each Staff in
-- the original Score. This data comes partly from a text configuration file
-- and partly from my executable command line.


-- Combines information from the config file and from the command line, such
-- as tempo change and measure range.


{-

data PlaybackConfig = PlaybackConfig
  { pcStaffSts            :: Map String StaffConfig
  , pcStaffIgnore         :: Set String
  -- , pcInstrs              :: Map String Instrument
  , pcSusPed              :: Maybe SusPedConfig
  , pcTVar                :: Maybe TimingVariation
  , pcPattern             :: PatternData
  , pcTempoRatio          :: Double
  , pcTranspose           :: Int
  , pcMsrRange            :: (Int,Maybe Int)
  , pcSplice              :: Maybe Char
  }


data PlaybackConfig = PlaybackConfig
  { pcMiSts      :: Map String MiStatement
  , pcExprSts    :: [ExprStatement]
  , pcTempoRatio :: Double
  , pcMsrRange   :: (Int,Maybe Int)
  }

-}


  
----------------------------------------------------------------------

-- An Instrument. This is a data structure which includes functions that
-- handle the specifics of computing loudness, note timing and duration,
-- etc. which are application to the specific Instrument.

--
-- timeFn :: <note> -> (tBeg,tEnd,model)
--
-- velFn  :: <case> -> <loudness at begin loc> -> <midi velocity>
--
-- destFn :: <case> -> <chord> -> <midi stream and channel
--   destination of the notes in this particular chord>
--
-- loudnessCurvesFn :: <staff name> -> <list of LoudnessCurve that result 
--    from the ways in which this particular instrument interprets the Score>
--
-- trillTremRateFn  :: <number of notes per second in trills and tremolos>
--
-- modDestFn :: [<midi stream and channel that modulation events should be
--   sent to>] (If this is Nothing, then this Instrument does not use
--   modulation events.) There is a list of midi stream/channel pairs because
--   some instruments will require continuous loudness controllers on more
--   than one channel
--
-- modValueFn :: <loudness value as floating point> -> [<midi integer scaled
--   from 0 to 127 as used in the mod channel>]. The list of values
--   corresponds to the list of mod destination stream/channels.
--
-- instrParamTypes  :: [(<param name>, <param value type>)] -- list of
--   parameters that should appear in the config file
--
--
-- How do we handle computing modifiers?
--
--   problem: if we determine the modifiers taking each note by itself, then
--   we will create the same modifiers for every note in a chord
--
--     solution: remove redundant modifiers in MakeRaw.hs
--
--   writing iModifierFn
--
--     Quantum Leap: all the following modifiers are note-off modifiers
--
--       in SingleCase
--
--         if destination is to a sustained note channel
--
--           if there is no gap with following note, and it's under a slur,
--           then generate control codes for legato. otherwise turn off legato
--
--       in TrillCase or TremoloCase
--
--         if destination is to sustained note channel, issue control codes
--         for legato, otherwise no control code
--
--
--     Garritan: all of these are note on modifiers.
--
--   
-- 

{-
data Instrument = Instrument
  { iTimeFn           :: NoteKey -> Tr TimeModel
                         -- <case> <loudness>:
  , iVelFn            :: TranslationCase -> Double -> ChordKey -> 
                         Tr Int
  , iDestFn           :: TranslationCase -> ChordKey -> ([(Int,Int)],Bool) 
                         -- ^ output is ( [(<stream>,<chan>)]
                         --             , <true if sustained type sound>
                         --             )
                         -- sustained type sound is any patch that requires
                         -- a separation between notes of that patch in order
                         -- to clearly rearticulate the same pitch
  , iModifierFn       :: TranslationCase -> ChordKey ->
                         ([RawMidiEvent],[RawMidiEvent])
                         -- ^ output is ( <note-on  modifiers>
                         --             , <note-off modifiers> )
  -- , iMakeLoudnessFn   :: AbsTimeMap -> Map Int LoudnessFunc
  , iTrillShape       :: Map Loc TrillShape
  , iTremShape        :: Map Loc TrillShape
  , iAlterTOff        :: TruncExtConfig
  , iModConfig        :: Maybe ModConfig
  , iSusPedDest       :: Maybe (Int,Int)
                         -- The following are controls values that must be
                         -- send at the start, for some destinations.
                         -- ( <dest>, (<contr number>, <value>))
  , iFixedControls    :: [((Int,Int),(Int,Int))]
  }

-}


data ModConfig = ModConfig
  { mcDest        :: (Int,Int)
  , mcControllers :: [Int]
  , mcValueFn     :: Double -> [Int]
  , mcSpacing     :: Double
  , mcLead        :: Double
  }

----------------------------------------------------------------------
----------------------------------------------------------------------
--           data needed for translation: time map


-- 
data RelTimeMap = RelTimeMap (Map Loc Double)

data AbsTimeMap = AbsTimeMap (Map Loc Double)

----------------------------------------------------------------------
----------------------------------------------------------------------
--  data needed for translation: dynamics and loudness functions

type LoudnessFunc = Either Loc Double -> Double

data DynSeg a = DynSeg Double Double a
            deriving(Show)

-- data DynSegD = DynSegD Double Double Double

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
                       , pcmTVar :: Maybe TimingVariation
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
  , tsCmdLine    :: PlayCmd
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


{-

data TrState = TrState
  -- input state
  { tsPlaybackConfig  :: PlaybackConfig
  , tsScore           :: Score
  , tsRandomGen       :: StdGen

  -- computed state used in producing output
  , tsAbsTimeMaps     :: Map String AbsTimeMap
  , tsLoudFuncs       :: Map String StaffLoudnessFunc
  , tsMsrRange        :: (Int,Int)

  -- state that is folded while translating
  , tsMis             :: Map String MetaInstr

  -- converted note output
  , tsOutput          :: [[MidiEvent]]

                         
  -- debug output state
  , btsDebugOutput     :: [DebugItem]
  , btsLoudDebug       :: Map String [LoudnessDebug]
  , btsUnitTimeMods    :: [UnitTimeMod]
  }


-}

{-
data PerStaff = PerStaff
  { tpsTimeMap       :: AbsTimeMap
  , tpsLoudnessFuncs :: Map Int LoudnessFunc
  , tpsLoudnessDebug :: [LoudnessDebug]
  }
-}


{-
data DebugItem = 
  DebugNote
    { trChordKey    :: ChordKey
    , trCase        :: TranslationCase
    , trTimeModel   :: TimeModel
    , trPitch       :: PitchRecord
    , trBegLoudness :: Maybe Double
    }
  | DebugSusPed
    { trLoc :: Loc
    , trEvt :: PedalEvt
    , trPedStaffName :: String
    }
              deriving(Show)
-}

data FullDebugInfo = FullDebugInfo
  { fdiTimeMap :: Map String (AbsTimeMap,Map Int LoudnessFunc,[LoudnessDebug])
  , fdiUnitTimeMods :: [UnitTimeMod]
  }


data WrappedMidiEvent = WrappedMidiEvent MidiEvent



data PitchRecord = TrillTremPitchRecord [Int] [Int]
                 | SinglePitchRecord    Int
                   deriving(Show)


-- TrillTremModel <tBeg> <tEnd>


{-
data TimeModel = RegularModel {tmTBeg, tmTEnd :: Double}
               | StaccatoModel { 
-}


{-
isStacModel (StaccatoModel _ _) = True
isStacModel _                   = False
-}




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
--                  output

{-
tellDebugOutput :: DebugItem -> Tr ()
tellDebugOutput item = do
  o <- gets tsDebugOutput
  modify (\s -> s {tsDebugOutput = item:o})
-}


{-
tellMidiEvt :: MidiEvent -> Tr ()
tellMidiEvt item = do
  o <- gets tsMidiEvts
  modify (\s -> s {tsMidiEvts = item:o})
-}

{-
tellMidiEvt :: String -> MidiEvent -> Tr ()
tellMidiEvt staffName e = do
  m <- gets tsMidiEvts
  let t = meTime e
      new = M.alter g staffName $ m
      g Nothing  = Just $ M.fromList [(t,[e])]
      g (Just x) = Just $ M.insertWith (++) t [e] x
  modify (\s -> s {tsMidiEvts = new})


tellMod :: String -> [MidiEvent] -> Tr ()
tellMod staffName evts = do
  m <- gets tsModEvts
  modify (\s -> s {tsModEvts = M.insert staffName evts m})
-}

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
          , miTranslateCk   :: MetaInstr -> ChordKey -> Tr ()
          , miTranslateMark :: MetaInstr -> StaffName -> Loc -> Mark -> Tr ()
          , pnoDests        :: Map String (Int,Int)
          , miStacDur       :: Map String Double
          , miArpDelta      :: Double
          , miTrillShape    :: Map String TrillShape
          , miTremShape     :: Map String TrillShape
          }
  

