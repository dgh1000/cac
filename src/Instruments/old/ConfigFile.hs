
module Instruments.ConfigFile where

import System.Random
import Control.Monad.State
import Control.Monad.Trans.State
import Data.Map(Map)

-- curves: create curves. first step, before interpreting score items one by
-- one? I think so. curve called "loud-" plus staff name will be used for
-- loudness

data Timing = Timing


data NoteSpec = NoteSpec
  { extend :: NoteKey -> Tr (Maybe Double)
  , trunc  :: NoteKey -> Tr (Maybe Double)
  , delay  :: NoteKey -> Tr (Maybe Double)
  , vel    :: NoteKey -> Tr Int
  , ks     :: NoteKey -> Tr Int
  , modif  :: NoteKey -> Tr [(Int,Int,Int)]
  }

dat

data ConfigFile = ConfigFile [Meta] TimingVariation

data MiState = MiState
  { misCfs    :: Map String ConfigValue
  , misCurves :: Map String [OneCurve]
  }

data Dests = Dests ((String,(Int,Int)) (NoteKey -> (Int,Int))

-- wait we still want to produce one data at a time, convert one, except for
-- time maps, how do we update state? produce pairs that will update state
-- upon termination? or directly work on state in state monad? or know
-- instrument name?
                    
data Meta = Meta
  { name        :: String
  , init        :: I (Map String Value,[MidiEvent])
  , staves      :: [String]
  , dests       :: String -> I (Int,Int)
  -- , alterT      :: Map ChordKey Timing -> I (Map ChordKey Timing)
  , timing      :: I (Map ChordKey Timing)
  , state       :: ScoreObject -> MiState -> MiState
  , continuous  :: Curves -> Tr [MidiEvent]
  }

-- do 

data Dest = Dest String Int Int

data Param = ParamI String Int
           | ParamD String Double
           | ParamS String String
             
