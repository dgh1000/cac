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


data MiState = MiState (Map String ConfigValue)


data Chans = Chans [(Int,Int)]


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




tdMapLookup :: Ord k => k -> Map k a -> a
tdMapLookup k m = case M.lookup k m of {Just x -> x}



data Meta = Meta
  { name     :: String
  , init     :: Mr (Map String Value,[MidiEvent])
  , staves   :: Mr [String]
  , dests    :: Mr (String -> (Int,Int))
  , timing   :: Br (Map ChordKey Timing)
  }


  



