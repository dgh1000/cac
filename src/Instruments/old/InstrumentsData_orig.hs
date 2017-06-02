{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module Instruments.InstrumentsData where

import qualified Data.Map as M
import System.Random
import Data.Map(Map)
import Control.Monad.State
import Common.CommonData
import Score.ScoreData
import Translation.TranslationData
import Midi.MidiData

----------------------------------------------------------------------
data PlayCmd = PlayCmd { pcmMsrRange :: (Int,Maybe Int)
                       , pcmTempoRatio :: Double
                       , pcmTranspose :: Int
                       }


----------------------------------------------------------------------

-- how would we create a function that modifies loudness? or any curve?

-- something like "get note times that have certain articulation, create and
-- add in curve"


----------------------------------------------------------------------

data ChordTiming = ChordTiming Bool [(Int,OnOff)]


type Tr = State TrState


data ScoreTiming = ScoreTiming
  { sctStaves :: Map String StaffTiming
  }


data StaffTiming = StaffTiming
  { sttChords :: Map Loc (Map Int ChordTiming)
  }


data ChordTiming = ChordTiming
  { ctNotes :: Map Int NoteTiming
  }


data NoteTiming = SingleTiming OnOff
                | TrillTiming [OnOff]


data MiState = MiState (Map String Value)


data Chans = Chans [(Int,Int)]


type ScoreTiming = Map NoteKey NoteTiming


data TrState = TrState
  -- input configuration
  { tsConfigFile :: ConfigFile
  , tsPlayCmd    :: PlayCmd
  , tsScore      :: Score
                    
  -- random generator                   
  , tsRandomGen  :: StdGen

  -- computed state used in producing output
  , tsTiming      :: Map ChordKey Timing
  , tsAbsTimeMaps :: Map String AbsTimeMap
  , tsChordChans  :: Map ChordKey Chans
  , tsLoudFuncs   :: Map String StaffLoudnessFunc
  -- , tsMsrRange    :: (Int,Int)

  -- meta-instruments. key is meta instrument name (not type)
  -- , tsMis        :: Map String MetaInstr
  , tsMiState       :: Map String MiState
                    
  -- debug
  , tsLoudnessDebugs   :: Map String [LoudnessDebug]
  , tsUnitTimeMods     :: [UnitTimeMod]
                    
  -- converted note output
  , tsOutput          :: [[MidiEvent]]
  }


prependOutputTr :: [MidiEvent] -> Tr ()
prependOutputTr evts =
  modify (\s@TrState {tsOutput = xs} -> s {tsOutput = evts:xs})

----------------------------------------------------------------------

instance RandomState Tr where
  getGen = gets tsRandomGen
  putGen g = modify (\s -> s {tsRandomGen=g})



----------------------------------------------------------------------



class MetaInstrClass a where
  nameG      :: a -> String
  stacDurS   :: String -> Double -> a -> a
  stacDurG   :: String -> a -> Double
  arpDeltaS  :: Double -> a -> a
  arpDeltaG  :: a -> Double
  trillShapeS :: String -> TrillShape -> a -> a
  trillShapeG :: String -> a -> TrillShape
  tremShapeS  :: String -> TrillShape -> a -> a
  tremShapeG  :: String -> a -> TrillShape


instance MetaInstrClass MetaInstr where
  nameG = cName . miCommon
  stacDurG stName = tdMapLookup stName . cStacDur . miCommon
  arpDeltaG = cArpDelta . miCommon
  trillShapeG stName = tdMapLookup stName . cTrillShape . miCommon
  tremShapeG stName = tdMapLookup stName . cTremShape . miCommon

  stacDurS staffName x mi =
    let c = miCommon mi
        m = cStacDur c
    in mi {miCommon = c {cStacDur = M.insert staffName x m}}
  arpDeltaS x mi =
    let c = miCommon mi
    in mi {miCommon = c {cArpDelta = x}}
  trillShapeS staffName x mi =
    let c = miCommon mi
        m = cTrillShape c
    in mi {miCommon = c {cTrillShape = M.insert staffName x m}}
  tremShapeS staffName x mi =
    let c = miCommon mi
        m = cTremShape c
    in mi {miCommon = c {cTremShape = M.insert staffName x m}}


tdMapLookup :: Ord k => k -> Map k a -> a
tdMapLookup k m = case M.lookup k m of {Just x -> x}




data MetaInstrCommon = MetaInstrCommon
  { cName            :: String
  , cStaffNames      :: [String]
  , cOrigConfig      :: Elem
  , cInitTrack       :: MetaInstr -> [(Int,Int,Int,Int,Int)]
  , cTranslateCk     :: MetaInstr -> ChordKey -> Tr ()
  , cTranslateMark   :: MetaInstr -> String -> Loc -> MarkD -> Tr ()
  , cStacDur         :: Map String Double
  , cArpDelta        :: Double
  , cTrillShape      :: Map String TrillShape
  , cTremShape       :: Map String TrillShape
  }                                   


data QConfig = QConfig
  { qcName              :: String
  , qcHasPizz           :: Bool
  , qcShortDestForStac  :: Bool
  , qcShortDestForTrill :: Bool
  , qcVelRanges         :: Map String (Int,Int)
  }


data MetaInstr = MiPiano
  { miCommon        :: MetaInstrCommon
  , pnoDests        :: Map String (Int,Int)
  , pnoLegOvlp      :: Maybe Double
  }
               | MiQ
  { miCommon        :: MetaInstrCommon
  , qTechnique      :: Map String String
  , qDests          :: Map String (Int,Int)
  , qLegOvlp        :: Maybe Double
  , qSepSame        :: Maybe Double
  }
  



