
module Instruments.ConfigFile where

-- curves: create curves. first step, before interpreting score items one by
-- one? I think so. curve called "loud-" plus staff name will be used for
-- loudness

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

data Meta = Meta
  { name        :: String
  , init        :: Tr (Map String Value,[MidiEvent])
  , staves      :: [String]
  , dests       :: Dests String -> (Int,Int)
  , alterT      :: Map ChordKey Timing -> B (Map ChordKey Timing)
  , timing      :: Tr (Map ChordKey Timing)
  , loudness    :: Tr [(Map String OneCurve)]
  , vel         :: Tr (Map ChordKey Vel)
  , 
  , spec        :: NoteSpec
  , curves      :: Tr (Map String OneCurve)
  , init        :: Tr ([MidiEvent],MiState)
  , state       :: ScoreObject -> MiState -> MiState
  , continuous  :: Curves -> Tr [MidiEvent]
  }

-- do 

data Dest = Dest String Int Int

data Param = ParamI String Int
           | ParamD String Double
           | ParamS String String
             
