{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveAnyClass ,
DeriveGeneric #-}


module Instruments.InstrumentsData where

import qualified Data.Map as M
import Text.Printf
import System.Random
import Data.Map(Map)
import Data.Maybe
import GHC.Generics hiding(Meta)
import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Common.CommonData
import Score.ScoreData
import Util.Exception
import Util.Showable

----------------------------------------------------------------------
--                 time-related

data RelTimeMap = RelTimeMap (Map Loc Double)

data AbsTimeMap = AbsTimeMap (Map Loc Double)

type StaffMarkers = Map String Markers

-- 
data Context = Context [String] (Map Int TimeSig) StaffMarkers 

type ConMark = (String,(Loc,MarkD))  -- staff name, plus MarkD





-- UnitWarp with the Either field as Left:
--
--    the first Loc is at the warp mark.
--
-- UnitAbsWarp:; the first Loc is at the warp mark
data UnitTimeMod = -- UnitRamp  Loc Loc Double Double
                   UnitPause Loc Rational
                 | UnitWarp (Maybe String) (Either (Loc,Loc) (Loc,Loc,Loc))
                            Rational
                 | UnitAbsWarp Loc Loc Rational
                 | Unit2Modify Loc Loc Loc TempoModify TempoModify
                               (Maybe Double)
                     -- <loc1> <loc2> <loc3> <tm1> <tm2> <normalize fraction>
                     --
                     -- Make two regions: loc1 to loc2, and loc2 to loc3. In
                     -- first region, modify tempo according <tm1>
                     -- instructions. in second region, modify according to
                     -- <tm2>. Then restore overall duration according to
                     -- <normalize fraction>. If this is 1, restore orginal
                     -- duration precisely. If this is something between 0 and
                     -- 1, take that fraction of the difference in tempo and
                     -- restore that amount.
                 | UnitAdjust (Maybe String) Loc Loc Loc Rational


isPause (UnitPause _ _)             = True
isPause _                           = False

isAdjust (UnitAdjust _ _ _ _ _)     = True
isAdjust _                          = False

isWarpLocal (UnitWarp m _ _)        = isJust m
isWarpLocal _                       = False

isWarpGlob (UnitWarp m _ _)         = isNothing m
isWarpGlob _                        = False

isAbsWarp (UnitAbsWarp _ _ _)       = True
isAbsWarp _                         = False

isTwoModify (Unit2Modify _ _ _ _ _ _) = True
isTwoModify _                         = False

----------------------------------------------------------------------

data OneCurve = OneCurve (Map Double Seg) TimeSigs (Map Double Loc)
data Seg = Seg Double Double Double  -- <end> <value1> <value2>
data Curve = Curve [OneCurve] 


---------------------------------------------------------------------

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
  deriving(Show)


----------------------------------------------------------------------
type Tr = ExceptT String (State TrState)

data Modif = ModifKs
  { modTiming :: Either Double Double  -- Left d means timing relative to
                 -- on time, Right d means timing relative to off time
  , modKey    :: Int
  }
           | ModifCtrl
  { modTiming :: Either Double Double
  , modCtrl   :: Int
  , modValue  :: Int
  }


data DestData = DestData
  { ddChanNum  :: (Int,Int)
  , ddPitch    :: Int
  , ddVel      :: Int
  , ddMods     :: [Modif]
  }


data TrNote = TrSingle
  { tnStaffName :: String
  , tnLoc       :: Loc
  , tnEndLoc    :: Loc
  , tnVn        :: Int
  , tnChord     :: Chord
  , tnNote      :: Note
  , tnOnOff     :: OnOff
  , tnDests     :: [DestData]
  , tnNomPitch  :: Int
  -- , tnChanNums  :: (Int,Int)
  -- , tnPitch     :: Int
  , tnLoud      :: Double
  -- , tnVel       :: Int
  -- , tnMods      :: [Modif]
  , tnAlterEnd  :: Double
  , tnSepSame   :: Double
  }
            | TrTrill
  { tnStaffName :: String
  , tnLoc       :: Loc
  , tnEndLoc    :: Loc
  , tnVn        :: Int
  , tnChord     :: Chord
  , tnIdx       :: Int
  , tnCount     :: Int
  , tnOnOff     :: OnOff
  , tnDests     :: [DestData]
  , tnNomPitch  :: Int
  -- , tnChanNums  :: (Int,Int)
  -- , tnPitch     :: Int
  , tnLoud      :: Double
  -- , tnVel       :: Int
  -- , tnMods      :: [Modif]
  , tnAlterEnd  :: Double
  , tnSepSame   :: Double
  }

data TrRaw = TrRaw
  { trStaffName :: String
  , trTime      :: Double
  , trDest      :: (Int,Int)
  , trStatus    :: Int
  , trData1     :: Int
  , trData2     :: Int
  }


data TrState = TrState
  -- constant
  { tsScore  :: Score
  , tsMetas  :: Map String Meta
  , tsTimVar :: TimingVariation

  -- random generator             
  , tsGen    :: StdGen

  -- computed
  , tsTimeMaps    :: Map String AbsTimeMap
  , tsLoudness    :: Map String (Map Int Curve)
                     
  -- output
  , tsNotes    :: [[TrNote]]
  , tsRaws     :: [[TrRaw]]
  , tsInitRaws :: [[TrRaw]]
  , tsTimeMods :: Map String [UnitTimeMod]
  }


----------------------------------------------------------------------

data OnOff = OnOff [(String,(Double,Double))]
           deriving(Show,Eq,NFData,Generic)


headTimes :: OnOff -> (Double,Double)
headTimes (OnOff xs) = case xs of
  (_,(t1,t2)):_ -> (t1,t2)


onTime :: OnOff -> Double
onTime t = let (b,_) = headTimes t in b


offTime :: OnOff -> Double
offTime t = let (_,e) = headTimes t in e


consTimes :: String -> Double -> Double -> OnOff -> OnOff
consTimes s t1 t2 (OnOff xs) = OnOff $ (s,(t1,t2)):xs

----------------------------------------------------------------------
data RunData = RunData [Meta] TimingVariation

----------------------------------------------------------------------

instance RandomState Tr where
  getGen = gets tsGen
  putGen g = modify (\s -> s {tsGen=g})



----------------------------------------------------------------------


type StaffCurves = Map String (String,Curve)
                   -- map of curve name to (<control name>, <curve data>)
                  
data MetaCommon = MetaCommon
  { mcName         :: String
  , mcStaffNs      :: [String]
  , mcTremShapes   :: Map String (Map Loc TrillShape)
  , mcTrillShapes  :: Map String (Map Loc TrillShape)
  , mcStacDurs     :: Map String (Map Loc Double)
  , mcArtics       :: Map String (Map Loc String)
  , mcArpDelta     :: Map Loc Double
  , mcLegExt       :: Map String (Map Loc Double)
  , mcTrunc        :: Map String (Map Loc Double)
  , mcCurves       :: Map String StaffCurves
                      -- map of staff name to StaffCurves
  }
  
----------------------------------------------------------------------

data Piano = Piano
  { pnoInit        :: Piano -> Tr Piano
  , pnoRun         :: Piano -> Int -> Int -> Tr ()

  -- piano specifc configuration
  , pnoDests       :: Map String (Int,Int)
  , pnoVCurveSingle:: VelCurve
  , pnoVCurveTrill :: VelCurve

  , pnoCommon      :: MetaCommon

  }

----------------------------------------------------------------------


data VelCurve = VelCurve [(Double,Double)]

data Q = Q
  { qInit         :: Q -> Tr Q
  , qRun          :: Q -> Int -> Int -> Tr ()

  -- q specific configuration
  , qStaffN       :: String

  , qCommon       :: MetaCommon
  }


data QAlias = QaKs String
            | QaShort


data QConfig = QConfig
  { qbKsPitches     :: [(String,Int)]
  , qbKsVels        :: [(String,VelCurve)]
  , qbShortVel      :: VelCurve
  , qbAliases       :: [(String,QAlias)]
  , qbDestFns       :: [Q -> QConfig -> TrNote -> Tr (Maybe DestData)]
  , qbInits         :: [((Int,Int),(Int,Int))] -- chan nums paired with
                                               -- initial volume and
                                               -- expression
  }

----------------------------------------------------------------------

-- moog organization
--
--   Reaper TRACKS: each one holds a MOOG INSTANCE configured to a particular
--   PATCH (wiring arrangement). A Moog instance can change many control
--   values that affect its output sounds; a set of control values together
--   with the specific patch is an ARTICULATION.
--
--   each TRACK will be fed from one MIDI STREAM... the MOOG INSTANCE at that
--   track will be configured to receive control and note messages from all 16
--   tracks.
--
-- staves
--
--   a moog meta-instrument will encompass multiple staves. a note on any staff
--   could go to any the tracks, after setting the patch at that track to any
--   articulation, the articulation mark
--
--   a staff Artic marking will indicate what Moog articulation to use; this
--   will determine which Reaper track has the right patch, and whether it
--   needs its controls configured.
-- 
--
-- data we need to keep with each Moog
--
--   control definitions:
--
--     example: "lo-pass frequency" -- we will store a channel and MIDI
--     controller number
--
--     there is a master list of controls. these will apply to any Moog PATCH
--
--   articulation definition
--
--     contains patch name and control values
--
--   MoogTrack
--
--     contains stream number and patch name
--
-- control points: curves are associated with a particular staff, but a staff
-- could potentially be directed to different tracks or channels. so we need
-- to decide what stream to send a curve to.. for now, to all.
--
-- CONTROL CURVES
--
--   how configured...
--
--     a Moog is created with a function 


{-
data MoogTrack = MoogTrack Int String -- <stream> <patch present here>
-}

data MoogArtic = MoogArtic String [(String,Int)] VelCurve
                 -- <patch name>, list of <ctrl name, ctrl value>, <velcurve>

{-
data MoogLayout = MoogLayout
  { mlTracks  :: [Map Loc (String,Loc)]
                 -- for each track, an indication of which segments loc1 ->
                 -- loc2 have been allocated to which articulation
  }
-}

type StSectionLayout = Map Loc (Loc,Int) -- <beg loc> (<end loc>,<track num>)

type TrkSectionLayout = Map Loc (Loc,String) -- <beg loc> (<end loc>, <artic>)

data LayoutState = LayoutState (Map String StSectionLayout)
                               (Map Int TrkSectionLayout)

data Moog = Moog
  { mgInit          :: Moog -> Tr Moog
  , mgRun           :: Moog -> Int -> Int -> Tr ()
  , mgCommon        :: MetaCommon
  , mgTrks          :: [Int]   -- list of <stream> for tracks
  , mgCtrlNums      :: [(String,(Int,Int))] -- map of control name to MIDI
                                            -- channel and controller number
  , mgArtics        :: [(String,MoogArtic)]
  , mgLayout        :: LayoutState
  , mgGenrCurves    :: [(String,(String,Curve))]
                       -- [(<curve name>,(<control name>,<curve>))]
  , mgCurveAssign   :: [(String,String)] -- [(<curve name>, <control name>)]
  }


----------------------------------------------------------------------

data Meta = MetaQ Q | MetaPiano Piano | MetaMoog Moog


mCommon :: Meta -> MetaCommon
mCommon (MetaPiano p) = pnoCommon p
mCommon (MetaQ q    ) = qCommon q
mCommon (MetaMoog m ) = mgCommon m


mSetCommon :: MetaCommon -> Meta -> Meta
mSetCommon c (MetaPiano p) = MetaPiano  p {pnoCommon = c}
mSetCommon c (MetaQ     q) = MetaQ      q {qCommon   = c}
mSetCommon c (MetaMoog  m) = MetaMoog   m {mgCommon  = c}


mName :: Meta -> String
mName = mcName . mCommon


mStaffNs = mcStaffNs . mCommon

mTremShapes = mcTremShapes . mCommon

mSetTremShapes :: Map String (Map Loc TrillShape) -> Meta -> Meta
mSetTremShapes s m = let x = mCommon m in mSetCommon x {mcTremShapes = s} m


mTrillShapes = mcTrillShapes . mCommon

mSetTrillShapes :: Map String (Map Loc TrillShape) -> Meta -> Meta
mSetTrillShapes s m = let x = mCommon m in mSetCommon x {mcTrillShapes = s} m


mStacDurs = mcStacDurs . mCommon

mSetStacDurs :: Map String (Map Loc Double) -> Meta -> Meta
mSetStacDurs d m = let x = mCommon m in mSetCommon x {mcStacDurs = d} m


mArtics = mcArtics . mCommon

mSetArtics :: Map String (Map Loc String) -> Meta -> Meta
mSetArtics a m = let x = mCommon m in mSetCommon x {mcArtics = a} m


mArpDelta = mcArpDelta . mCommon

mSetArpDelta :: Map Loc Double -> Meta -> Meta
mSetArpDelta d m = let x = mCommon m in mSetCommon x {mcArpDelta = d} m


mLegExt = mcLegExt . mCommon

mSetLegExt :: Map String (Map Loc Double) -> Meta -> Meta
mSetLegExt d m = let x = mCommon m in mSetCommon x {mcLegExt = d} m


mTrunc = mcTrunc . mCommon

mSetTrunc :: Map String (Map Loc Double) -> Meta -> Meta
mSetTrunc d m = let x = mCommon m in mSetCommon x {mcTrunc = d} m


  
initMeta :: Meta -> Tr Meta
initMeta (MetaPiano p) = MetaPiano <$> pnoInit p p
initMeta (MetaQ q)     = MetaQ <$> qInit q q
initMeta (MetaMoog m)  = MetaMoog <$> mgInit m m


runMeta :: Meta -> Int -> Int -> Tr ()
runMeta (MetaPiano p) mBeg mEnd = pnoRun p p mBeg mEnd
runMeta (MetaQ q)     mBeg mEnd = qRun q q mBeg mEnd
runMeta (MetaMoog m)  mBeg mEnd = mgRun m m mBeg mEnd

----------------------------------------------------------------------

tdMapLookup :: Ord k => k -> Map k a -> a
tdMapLookup k m = case M.lookup k m of {Just x -> x}
