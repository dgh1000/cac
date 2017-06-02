
module Instruments.Q where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Map(Map)
import Data.Maybe
import Text.Printf
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Common.CommonUtil
import Instruments.AlterTimes
import Util.Math
import Util.Exception


-- QuantumLeap is different from piano in the following
--
--   more than one possible dest per staff
--
--   sometimes an articulation will be selected by keyswitch, sometimes by
--   choosing dest
--
--   sometimes we need to know the score articulation (like accent or
--   staccato) to determine the destination
--
-- 

-- so we need some way to describe a channel which is term for what is loaded
-- at a particular channel. we need abstracted concept of what sound or
-- instrument is at a channel, which could be called "channel idea".
--
-- we need some way to describe a keyswitch instrument independent
--
-- so channel idea might be "short-idea". instrument would be Quantum Leap
-- solo viola, articulation susLeg.
--
-- we need mapping of
--
-- ks-channel: q/solo viola/ks
--
-- short-channel:
--
-- +arco = ks-channel#susLeg
--
-- +expr = ks-channel#
-- stac
--
-- #susLeg
--
--
-- what do we need to create an instrument?
--
-- for each note,
--
--   choose channel
--
--   choose if note is shortened for staccato and how much
--
--   choose 
--
-- 

-- what do we need

violaKSMap = M.fromList [ ("qSoloViola/susVib"      , 24)
                        , ("qSoloViola/nonVibRR"    , 25)
                        , ("qSoloViola/qLeg"        , 26)
                        , ("qSoloViola/susLeg"      , 27)
                        , ("qSoloViola/expLeg"      , 28)
                        , ("qSoloViola/trillHT"     , 29)
                        , ("qSoloViola/TrillWT"     , 30)
                        , ("qSoloViola/exp1"        , 31)
                        , ("qSoloViola/exp2"        , 32)
                        , ("qSoloViola/exp3"        , 33)
                        , ("qSoloViola/expVibSft"   , 34)
                        , ("qSoloViola/martRR"      , 35)
                        , ("qSoloViola/marcHardRRx2", 36)
                        , ("qSoloViola/pizzRR"      , 37)
                        , ("qSoloViola/spicRRx2"    , 38)
                        , ("qSoloViola/colLegnoRR"  , 39)
                        , ("qSoloViola/8vsSlideUp"  , 40) ]

celloKSMap = M.fromList [ ("qSoloCello/dblBowExp"   , 30)
                        , ("qSoloCello/expDn"       , 28)
                        , ("qSoloCello/expUp"       , 29)
                        , ("qSoloCello/expVib"      , 31)
                        , ("qSoloCello/nonVib"      , 27)
                        , ("qSoloCello/qLeg"        , 25)
                        , ("qSoloCello/susVibSmooth", 24)
                        , ("qSoloCello/susLeg"      , 26)
                        , ("qSoloCello/colLegnoRR"  , 91)
                        , ("qSoloCello/marc"        , 89)
                        , ("qSoloCello/martUpDn"    , 87)
                        , ("qSoloCello/pizzRR"      , 90) ]

bassKSMap  = M.fromList [ ("qSoloContrabass/susVib"        , 72)
                        , ("qSoloContrabass/qLeg"          , 73)
                        , ("qSoloContrabass/susLeg"        , 74)
                        , ("qSoloContrabass/expLeg"        , 75)
                        , ("qSoloContrabass/lyrLeg"        , 76)
                        , ("qSoloContrabass/susNV"         , 77)
                        , ("qSoloContrabass/lyrical"       , 78)
                        , ("qSoloContrabass/exp"           , 79)
                        , ("qSoloContrabass/marcato"       , 80)
                        , ("qSoloContrabass/martele"       , 81)
                        , ("qSoloContrabass/pizz"          , 82)
                        , ("qSoloContrabass/spic"          , 83)
                        , ("qSoloContrabass/colLegno"      , 84)
                          


data QArtic = QArtic
  { qaName         :: String    -- example: qSoloViola/susLeg
  , qaVelCurve     :: MidiCurve
  }


data MidiCurves = MidiCurves MidiCurve (Maybe MidiCurve) (Maybe MidiCurve)


data KsMap = Map String Int     -- string is full name of articulation


data Instr = QOneInstr QArtic 
           | QKsInstr KsMap
             

             
-- name of where note is to be directed is "ks-qSoloViola/susLeg" or
--  "short"


data Channel = Channel
  { chName           :: String
  , chDest           :: (Int,Int)
  , chInstr          :: Instr
  , chInitVol        :: Int
  , chInitExpr       :: Int
  }

data StacStrategy (Maybe String) (Maybe (Double,Double))

data Q = Q
  { qName         :: String
  , qStaffNs      :: [String]
  , qInit         :: Q -> Tr Q
  , qRun          :: Q -> Int -> Int -> Tr Q

  -- q specific configuration
  , qStaffN       :: String
  , qType         :: String -- i.e. "qSoloViola", "qSoloCello". this will be
                            -- prefix to articulation name
  , qChans        :: Map String Channel
                     --  map of chan name to QMidiChan (channel info such as
                     --  stream/chan numbers [dest], init vol and expr). the
                     --  channel numbers here will change depending on how
                     --  Reaper is set up.
  , qArtics       :: Map String QArtic
                     -- map of articulation name to articulation description,
                     -- including keyswitch number (if one), channel name.
  , qStacArtic    :: String  -- channel, and if keyswitch includes artic name
  , qTimeAlter    :: (Double,Double,Double)
  , qScoreAliases :: [(String,Q -> String -> [String])]
                     -- map of score artic mark to function that produces a
                     -- list of channel-artic names where to direct it


  -- common configuration
  , qTremShapes   :: Map String (Map Loc TrillShape)
  , qTrillShapes  :: Map String (Map Loc TrillShape)
  , qStacDurs     :: Map String (Map Loc Double)
  , qArticMarks   :: Map String (Map Loc String)
  , qArpDelta     :: Map Loc Double
  }



{-

qCello dest =
  QData { qdInitVol    = 126
        , qdDest       = dest
        , qdKs         = [ ("arco", 24) -- C0
                         , ("marc", 89) -- F5
                         , ("mart", 88) -- E5
                         , ("pizz", 90) -- F#5
                         ]
        , qdShortArtic = "mart"
        }

-}


{-

data QData = QData
  { qdDests       :: Map String QDest  -- (init volume) (str,chan)
  , qdArtic       :: Map String QArticDesc
  , qdStacArtic   :: String  -- articulation to use for notes marked stac.
  }

-}

makeQViola name staffN midiChanNameToDest scoreAliasToArticName =
  makeQ name staffN (ext,gap,minDur) midiChans articDescrs
        scoreAliasToArticName "martRRx4Chan"
  where
    velCurve1 = VelCurve [ ((0,1),( 10,  10))
                         , ((1,8),( 10, 126))
                         , ((8,9),(126, 126)) ]
    velCurve2 = VelCurve [ ((0,1),( 10,  10))
                         , ((1,8),( 10,  64))
                         , ((8,9),( 64,  64)) ]
    mkKsMidiChan    =
      ("ks"  , QMidiChan  64 100 (qLookupL "ks"   midiChanNameToDest))
    mkShortMidiChan =
      ("short", QMidiChan 126 126 (qLookupL "short" midiChanNameToDest))
    midiChans = M.fromList [mkKsMidiChan,mkShortMidiChan]
    articDescrs = M.fromList
      [ ("susVib"       , QArticDescr (Just 24) "ks"  velCurve1) -- C0
      , ("nonVibRR"     , QArticDescr (Just 25) "ks"  velCurve1) -- C#0
      , ("qLeg"         , QArticDescr (Just 26) "ks"  velCurve1) -- D0
      , ("susLeg"       , QArticDescr (Just 27) "ks"  velCurve1) -- D#0
      , ("expLeg"       , QArticDescr (Just 28) "ks"  velCurve1) -- E
      , ("trillHT"      , QArticDescr (Just 29) "ks"  velCurve1) -- F0
      , ("trillWT"      , QArticDescr (Just 30) "ks"  velCurve1) -- F#0
      , ("exp1"         , QArticDescr (Just 31) "ks"  velCurve1) -- G0
      , ("exp2"         , QArticDescr (Just 32) "ks"  velCurve1) -- G#0
      , ("exp3"         , QArticDescr (Just 33) "ks"  velCurve1) -- A
      , ("expVibSft"    , QArticDescr (Just 34) "ks"  velCurve1) -- A#0
      , ("martRR"       , QArticDescr (Just 35) "ks"  velCurve1) -- B0
      , ("marcHardRRx2" , QArticDescr (Just 36) "ks"  velCurve1) -- C1
      , ("pizzRR"       , QArticDescr (Just 37) "ks"  velCurve1) -- C#1
      , ("spicRRx2"     , QArticDescr (Just 38) "ks"  velCurve1) -- D1
      , ("colLegnoRR"   , QArticDescr (Just 39) "ks"  velCurve1) -- D#1
      , ("8vsSlideUp"   , QArticDescr (Just 40) "ks"  velCurve1) -- E1
      , ("martRRx4Chan" , QArticDescr Nothing "short" velCurve2)
      ]
    ext    = 0.05
    gap    = 0.07
    minDur = 0.05


----------------------------------------------------------------------

makeQCello :: String -> String -> [(String,(Int,Int))] -> [(String,String)] ->
              Meta
makeQCello name staffN midiChanNameToDest scoreAliasToArticName =
  makeQ name staffN (ext,gap,minDur) midiChans articDescrs
        scoreAliasToArticName "martUpDnx6Chan"
  where
    velCurve1 = VelCurve [ ((0,1),( 10,  10))
                         , ((1,8),( 10, 126))
                         , ((8,9),(126, 126)) ]
    velCurve2 = VelCurve [ ((0,1),( 10,  10))
                         , ((1,8),( 10,  64))
                         , ((8,9),( 64,  64)) ]
    mkKsMidiChan    =
      ("ks"  , QMidiChan  74 126 (qLookupL "ks"   midiChanNameToDest))
    mkShortMidiChan =
      ("short", QMidiChan 126 126 (qLookupL "short" midiChanNameToDest))
    midiChans = M.fromList [mkKsMidiChan,mkShortMidiChan]
    articDescrs = M.fromList
      [ ("smooth" , QArticDescr (Just 24) "ks"   velCurve1)
      , ("qleg"   , QArticDescr (Just 25) "ks"   velCurve1)
      , ("leg"    , QArticDescr (Just 26) "ks"   velCurve1)
      , ("nonvib" , QArticDescr (Just 27) "ks"   velCurve1)
      , ("mart"   , QArticDescr (Just 88) "ks"   velCurve1)
      , ("marc"   , QArticDescr (Just 89) "ks"   velCurve1)
      , ("pizz"   , QArticDescr (Just 90) "ks"   velCurve1)
      , ("martUpDnx6Chan"   , QArticDescr Nothing "short" velCurve2)
      ]
    ext    = 0.05
    gap    = 0.07
    minDur = 0.05

----------------------------------------------------------------------

makeQBass :: String -> String -> [(String,(Int,Int))] -> [(String,String)] ->
              Meta
makeQBass name staffN midiChanNameToDest scoreAliasToArticName =
  makeQ name staffN (ext,gap,minDur) midiChans articDescrs
        scoreAliasToArticName "marcatoChan"
  where
    velCurve1 = VelCurve [ ((0,1),( 10,  10))
                         , ((1,8),( 10, 126))
                         , ((8,9),(126, 126)) ]
    velCurve2 = VelCurve [ ((0,1),( 10,  10))
                         , ((1,8),( 10,  64))
                         , ((8,9),( 64,  64)) ]
    mkKsMidiChan    =
      ("ks"  , QMidiChan  64 100 (qLookupL "ks"   midiChanNameToDest))
    mkShortMidiChan =
      ("short",
         QMidiChan 126 126 (qLookupL "short" midiChanNameToDest))
    midiChans = M.fromList [mkKsMidiChan,mkShortMidiChan]
    articDescrs = M.fromList
      [ ("susVib"  , QArticDescr (Just 72) "ks"   velCurve1) -- C4
      , ("qleg"    , QArticDescr (Just 73) "ks"   velCurve1) -- C#4
      , ("susLeg"  , QArticDescr (Just 74) "ks"   velCurve1) -- D4
      , ("expLeg"  , QArticDescr (Just 75) "ks"   velCurve1) -- D#4
      , ("lyrLeg"  , QArticDescr (Just 76) "ks"   velCurve1) -- E4
      , ("susNv"   , QArticDescr (Just 77) "ks"   velCurve1) -- F4
      , ("lyrical" , QArticDescr (Just 78) "ks"   velCurve1) -- F#4
      , ("exp"     , QArticDescr (Just 79) "ks"   velCurve1) -- G
      , ("marcato" , QArticDescr (Just 80) "ks"   velCurve1) -- G#4
      , ("martele" , QArticDescr (Just 81) "ks"   velCurve1) -- A
      , ("pizz"    , QArticDescr (Just 82) "ks"   velCurve1) -- A#4
      , ("spic"    , QArticDescr (Just 83) "ks"   velCurve1) -- B4
      , ("colLegno", QArticDescr (Just 84) "ks"   velCurve1) -- C5
      , ("marcatoChan", QArticDescr Nothing "short" velCurve2)
      ]
    ext    = 0.05
    gap    = 0.07
    minDur = 0.05


----------------------------------------------------------------------

    
qLookupL key lists = case lookup key lists of {Just x -> x}

makeQ :: String -> String -> (Double,Double,Double) ->
         Map String QMidiChan -> Map String QArticDescr -> [(String,String)] ->
         String -> Meta
makeQ name staffN alter midiChans articDescrs scoreAliases stacArtic =
  MetaQ
  Q { qName         = name
    , qStaffNs      = [staffN]
    , qInit         = qInit_impl
    , qRun          = qRun_impl

    
    , qStaffN       = staffN
    , qChans        = midiChans
    , qArticDescrs  = articDescrs
    , qStacArtic    = stacArtic
    , qTimeAlter    = alter
    , qScoreAliases = scoreAliases

    , qTremShapes   = M.empty
    , qTrillShapes  = M.empty
    , qStacDurs     = M.empty
    , qArtics       = M.empty
    , qArpDelta     = M.empty
    }

  
qInit_impl :: Q -> Tr Q
qInit_impl q = do
  let x :: QMidiChan -> [TrRaw]
      x (QMidiChan vol expr dest) =
        [ TrRaw (qStaffN q) 0 dest 0xB0 7 vol
        , TrRaw (qStaffN q) 0 dest 0xB0 11 expr ]
  includeInitRaws (concatMap x $ M.elems $ qChans q)
  return q


qRun_impl :: Q -> Int -> Int -> Tr Q
qRun_impl q mBeg mEnd = 
  (concat `liftM` forOneStaff nominalPitsTs (MetaQ q) (qStaffN q) mBeg mEnd)
    >>= mapM (runDest q)
    >>= mapM (runKs q)
    >>= mapM runLoud_common
    >>= mapM (runVel q)
    >>= (let (ext,gap,minDur) = qTimeAlter q
         in alterTOff ext gap minDur)
    >>= includeNotes
    >> return q


-- we can find the 'artic' easily enough. either the latest one, or the
-- staccato artic.
--
-- then we need to find the QDest associated with this artic.
--
-- the artic will have a dest alias

runDest :: Q -> TrNote -> Tr TrNote
runDest q note = do
  let (_,QMidiChan _ _ d) = qArticUtil q note
  return note {tnDest = d}
 



runKs :: Q -> TrNote -> Tr TrNote
runKs q note = do
  let (QArticDescr mKeyswitch _ _,_) = qArticUtil q note
  case mKeyswitch of
    Nothing -> return note
    Just k  ->
      return note {tnMods = ModifKs (Left (-0.05)) k:tnMods note}


  

qArticUtil :: Q -> TrNote -> (QArticDescr,QMidiChan)
qArticUtil q note = (articDescription,chan)
  where
  (scoreAlias,articName) = qArticUtil_getArtName q note
  articDescription = case M.lookup articName (qArticDescrs q) of
    Just x -> x
    Nothing -> throwMine $ printf ("problem in Q instrument '%s': no " ++
       "articulation description for articulation named '%s', referenced " ++
       "by score alias '%s'") (qName q) articName scoreAlias
  chan = case M.lookup (qadChanName articDescription) (qChans q) of
    Just x -> x
    Nothing -> throwMine $ printf ("artic name '%s' specifies " ++
      "channel name of '%s' but this is not in the Q configuration")
      articName (qadChanName articDescription)


-- Gets the articulation name.
--
-- If not a staccato note, looks up score articulation alias, then translates
-- that to articulation name.
--
-- If it is a staccato note, returns the articulation name that is
-- pre-configured for staccato notes.
--
qArticUtil_getArtName :: Q -> TrNote -> (String,String)
qArticUtil_getArtName q note
  | isShort (tnChord note) = ("<staccato symbol>", qStacArtic q)
  | otherwise = (scoreAlias,articName)
  where
    scoreAlias = case M.lookupLE (tnLoc note) . qLookup (tnStaffName note) $
                      qArtics q of
      Nothing -> throwMine $ printf ("no articulation alias at or " ++
                 "before loc %s") (showLoc2 $ tnLoc note)
      Just (_,a) -> a
    articName = case lookup scoreAlias (qScoreAliases q) of
      Nothing -> throwMine $ printf ("Q instrument '%s' is not configured "++
        "with a articulation name for score alias '%s'") (qName q)
        scoreAlias
      Just s -> s
      

runVel :: Q -> TrNote -> Tr TrNote
runVel q note = do
  let (QArticDescr _ name curve,_) = qArticUtil q note
  return note {tnVel = lookupVel ("in Q, with channel name "++name)
                       (tnLoud note) curve}

{-

qKs :: QData -> TrNote -> Tr TrNote
qKs (QData _ _ ksMap shortArtic) note = do
  result <-
      do
        VLocMap articMap <- lk $ staffData (tnStaffName note) ... "marks"
          ..."artic"
        let VString currArtic = qLookupLE (tnLoc note) articMap
        let currArticS =
              if isShort (tnChord note) then shortArtic else currArtic
        let r = lookup currArticS ksMap
        when (isNothing r)
          (throwError $ printf ("at %s, trying to use " ++
          "articulation '%s' but it's not in the keyswitch map")
          (showLoc2 $ tnLoc note) currArtic)
        return $ fromJust r
  return note {tnMods = ModifKs (Left (-0.05)) result:tnMods note} 
  
-}  
  
qLookupLE :: Ord k => k -> Map k a -> a
qLookupLE k m = case M.lookupLE k m of
  Just (_,v) -> v

qLookup :: Ord k => k -> Map k a -> a
qLookup k m = case M.lookup k m of
  Just x -> x



{-

data NoteContext = NcSingle String Loc Int Chord Note
                 | NcTrill String Loc Int Chord Note Loc Int Int


data KeySwitch = KsSingle Int
               | KsTrill Int (Maybe Double) -- Maybe Double: Nothing means
                                            -- keyswitch should be triggered
                                            -- once for whole duration of
                                            -- trill, Just t means trigger it
                                            -- at a specific time


-- keyswitch can switch between patch based on score mark, patch based on
-- articulation
selectKeySwitch :: NoteContext -> I (Maybe KeySwitch)
selectKeySwitch nc@(NcSingle staffName loc vn chord note) = do
  SQConf  conf <- lk $ metaState..."config"
  SString art  <- lk $ metaState..."articulation"
  let out | isShort chord = case lkSwitch "short" of
              Just s -> Just $ KsSingle s
          | otherwise = case lkSwitch art of
              Just s -> Just $ KsSingle s
              Nothing -> printf ("don't know what keyswitch to use with " ++
                         "articulation '%s'") `trace` Nothing
  return out
    

-- we'd like to compute timing knowing which note in the trill this is.
--
-- we want to have multi-pass algorithms. does that work when we are holding a
-- state and going note by note? yes it does... that state is only to decide
-- how to allocate notes, what the dests are. that could be done on a second
-- pass, really

computeTiming :: NoteContext -> OnOff
computeTiming (NcSingle staffName loc vn chord note) = do
  t1 <- lookupTimeI staffName loc
  t2 <- lookupTimeI staffName $ nTrueEnd note
  let oo1 = x 
-}
