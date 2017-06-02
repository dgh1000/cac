{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module Instruments.InstrumentsData where

import qualified Data.Map as M
import Text.Printf
import System.Random
import Data.Map(Map)
import Control.Monad.State
import Control.Monad.Reader
import Common.CommonData
import Score.ScoreData
import Translation.TranslationData
import Midi.MidiData
import Util.Exception
import Util.Showable


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


-- what is goal here? why not work with MidiEvents with times? well to one
-- extent it is good that we are coming up with a time independently of coming
-- up with dest. what do notes need to become reality? midi on/off, curves,

-- but realistically when do we ever come up a dest at a time different than
-- timing, when is one known but not the other?

-- we do want to have separate functions, but than can be expressed in
-- different ways

data Timing = Timing Bool [(Int,OnOff)]


type Tr = State TrState


type I = ReaderT Meta Tr


data Meta = Meta
  { metaName       :: String
    -- 'initialize' creates track start events and default values of any state
    -- that is needed
  , initialize :: I ()
  , staves     :: [String]
  , translate  :: ScoreObject -> I ()
  }


data Chans = Chans [(Int,Int)]


data TrState = TrState
  -- constant
  { tsScore  :: Score
  , tsMetas  :: Map String Meta
  , tsTimVar :: TimingVariation

  -- random generator             
  , tsGen    :: StdGen

  -- computed
  -- , tsTiming      :: Map ChordKey Timing
  , tsTimeMaps    :: Map String AbsTimeMap
  , tsStaffCurves :: Salue
  , tsMetaState   :: Salue
  , tsStaffState  :: Salue

  -- output
  , tsEvts     :: [[MidiEvent]]
  , tsInitEvts :: [[MidiEvent]]
  }


----------------------------------------------------------------------

data NoteContext = SingleContext
  { ncStaffName :: String
  , ncLoc       :: Loc
  , ncVn        :: Int
  , ncChord     :: Chord
  , ncNote      :: Note
  }
                 | TrillContext
  { ncStaffName :: String
  , ncLoc       :: Loc
  , ncVn        :: Int
  , ncChord     :: Chord
  , ncPitch     :: Int
  , ncTOn       :: Double
  }

----------------------------------------------------------------------

data Salue = SMap (Map String Salue)
           | STrillShape TrillShape
           | SDouble Double
           | SCurveList [OneCurve]
           | SCurveDblList [OneCurveDbl]
           | SEvents [MidiEvent]
           | SRawEvents [RawMidiEvent]
           | SDest (Int,Int)
             deriving(Show)

-- <meta instrs> <time var> 
data RunData = RunData [Meta] TimingVariation 


{-

class Adjustable a where
  adjust :: String -> (Salue -> Salue) -> a -> a
  insert :: String -> Salue -> a -> a


instance Adjustable Salue where
  adjust key f (SMap m) = case M.lookup key m of
    Nothing -> throwMine $ printf ("in Adjustable:adjust, tried to adjust " ++
               "at key '%s', but this key that is not present in the VMap")
               key
    Just x -> SMap $ M.insert key (f x) m
  adjust key _ _ = throwMine $ printf ("in Adjustable:adjust, while " ++
                 "attempting to adjust key '%s', the target for adjust " ++
                 "is not a VMap") key 
  insert key value (SMap m) = SMap $ M.insert key value m
  insert key _ _ = throwMine $ printf ("tried to insert into a Value " ++
                   "which is not a Map, at key '%s'") key

-}

{-

data SomeDataSet = StaffStateSet [String]
                 | MetaStateSet  [String]
                   deriving(Show)


data SomeDataSet = StaffStateSet [String]
                 | MetaStateSet  [String]
                   deriving(Show)
-}

data SomeData = StaffState [String]
              | MetaState  [String]
              | StaffCurves [String]


(...) :: SomeData -> String -> SomeData
(...) (StaffState  ss) s = StaffState  $ ss++[s]
(...) (MetaState   ss) s = MetaState   $ ss++[s]
(...) (StaffCurves ss) s = StaffCurves $ ss++[s]


{-
class SomeDataClass a where
  (...) :: a -> String -> I ([String],a)
  (===) :: ([String],a)
  


instance SomeDataClass SomeDataSet where
  (...) (StaffStateSet ss) s = StaffStateSet $ ss++[s]
  (...) (MetaStateSet  ss) s = MetaStateSet $ ss++[s]


staffStateSet = StaffStateSet []
metaStateSet  = MetaStateSet  []


instance SomeDataClass SomeDataLk where
  (...) (StaffStateLk ss) s = StaffStateLk $ ss++[s]
  (...) (MetaStateLk  ss) s = MetaStateLk  $ ss++[s]


staffStateLk = StaffStateLk []
metaStateLk  = MetaStateLk  []
-}




  
----------------------------------------------------------------------

instance RandomState Tr where
  getGen = gets tsGen
  putGen g = modify (\s -> s {tsGen=g})



----------------------------------------------------------------------

-- what about meta state, staff state?

{-

adjustMetaI :: String -> (Salue -> Salue) -> I ()
adjustMetaI key f = do
  metaName <- asks name
  s <- lift (gets tsMetaState)
  lift $ modify (\x -> x {tsMetaState = adjust key f s})


adjustStaffI :: String -> String -> (Salue -> Salue) -> I ()
adjustStaffI staffName key f = do
  s <- lift (gets tsStaffState)
  let x = adjust staffName (adjust key f) s
  lift $ modify (\y -> y {tsStaffState = x})


insertStaffI :: String -> String -> Salue -> I ()
insertStaffI staffName key v = do
  s <- lift (gets tsStaffState)
  let x = adjust staffName (insert key v) s
  lift $ modify (\y -> y {tsStaffState = x})

-}

----------------------------------------------------------------------




tdMapLookup :: Ord k => k -> Map k a -> a
tdMapLookup k m = case M.lookup k m of {Just x -> x}




  



