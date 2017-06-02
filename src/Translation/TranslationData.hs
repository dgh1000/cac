
module Translation.TranslationData where

import Data.Map(Map)
import Data.Maybe
import Common.CommonData
import Score.ScoreData


data PatternData = PatternData (Map Loc (Double,Double))  
  -- (<dynamic alteration>, <tempo ratio>)


----------------------------------------------------------------------
-- Any given Note has three translation scenarios.
data TranslationCase = SingleCase
                     | TrillCase (Int,Int)
                     | TremoloCase ([Int],[Int])
                       deriving(Show)


type TrillTremPitches = Either (Int,Int) ([Int],[Int])


----------------------------------------------------------------------
--                 dynamics-related

configApproxRational = 0.005

type LoudnessFunc = Either Loc Double -> Double

data DynSeg a = DynSeg Double Double a
            deriving(Show)

-- more general curves... we can easily think of cases in which they would be
-- expressed by time, and other cases by Loc... no have to do timemaps, no way
-- around it
--
-- why did I want to convert dynamic curves to double? do it all in Loc, back
-- convert times to Loc


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
--            Putting all playback configuration together

{-

data ConfigFile = ConfigFile
  { cfMis     :: [Elem]
  , cfTVar    :: Maybe TimingVariation
  }

-}




