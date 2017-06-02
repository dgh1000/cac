{-# LANGUAGE TupleSections #-}

module Instruments.Q where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Map(Map)
import Data.Maybe
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Score.ScoreData
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Common.CommonUtil
import Instruments.AlterTimes
import Util.Math
import Util.Exception


velCurveBasic = VelCurve [(0.45,10),(8.55,127)]

----------------------------------------------------------------------

mkCommon :: String -> [String] -> MetaCommon
mkCommon name staffNs = MetaCommon name staffNs M.empty M.empty M.empty
                                   M.empty M.empty M.empty M.empty 

----------------------------------------------------------------------

violaKSMap = [ ("susVib"      , 24)
             , ("nonVibRR"    , 25)
             , ("qLeg"        , 26)
             , ("susLeg"      , 27)
             , ("expLeg"      , 28)
             , ("trillHT"     , 29)
             , ("TrillWT"     , 30)
             , ("exp1"        , 31)
             , ("exp2"        , 32)
             , ("exp3"        , 33)
             , ("expVibSft"   , 34)
             , ("martRR"      , 35)
             , ("marcHardRRx2", 36)
             , ("pizzRR"      , 37)
             , ("spicRRx2"    , 38)
             , ("colLegnoRR"  , 39)
             , ("8vsSlideUp"  , 40) ]

makeQViola name staffName mainChan shortChan pianoChan aliases =
  MetaQ (Q (qInit_impl qConf) (qRun_impl qConf) staffName
            (mkCommon name [staffName]))
  where
    qConf = QConfig
      { qbKsPitches = violaKSMap
      , qbKsVels    = map ((,velCurveBasic) . fst) violaKSMap
      , qbAliases   = aliases
      , qbShortVel  = VelCurve [(0.45,10),(8.55,64)]
      , qbDestFns   = [ qMakeDest_main  0 mainChan
                      , qMakeDest_short 0 shortChan
                      , qMakeDest_piano 0 pianoChan
                      ]
      , qbInits     = [ (mainChan , ( 64 ,110))
                      , (shortChan, (127 ,127))
                      , (pianoChan, (127 ,127))
                      ]
      }

----------------------------------------------------------------------

celloKSMap = [ ("susVibSmooth", 24)
             , ("qLeg"        , 25)
             , ("susLeg"      , 26)
             , ("nonVib"      , 27)
             , ("expDn"       , 28)
             , ("expUp"       , 29)
             , ("dblBowExp"   , 30)
             , ("expVib"      , 31)
             , ("martUpDn"    , 87)
             , ("marc"        , 89)
             , ("pizzRR"      , 90) 
             , ("colLegnoRR"  , 91) ]

makeQCello name staffName mainChan shortChan pianoChan aliases =
  MetaQ (Q (qInit_impl qConf) (qRun_impl qConf) staffName
            (mkCommon name [staffName]))
  where
    qConf = QConfig
      { qbKsPitches = celloKSMap
      , qbKsVels    = map ((,velCurveBasic) . fst) celloKSMap
      , qbAliases   = aliases
      , qbShortVel  = VelCurve [(0.45,10),(8.55,64)]
      , qbDestFns   = [ qMakeDest_main  0 mainChan
                      , qMakeDest_short 0 shortChan
                      , qMakeDest_piano 0 pianoChan
                      ]
      , qbInits     = [ (mainChan , ( 60 ,110))
                      , (shortChan, (110 ,127))
                      , (pianoChan, (127 ,127))
                      ]
      }
                               


----------------------------------------------------------------------


bassKSMap  = [ ("susVib"        , 72)
             , ("qLeg"          , 73)
             , ("susLeg"        , 74)
             , ("expLeg"        , 75)
             , ("lyrLeg"        , 76)
             , ("susNV"         , 77)
             , ("lyrical"       , 78)
             , ("exp"           , 79)
             , ("marcato"       , 80)
             , ("martele"       , 81)
             , ("pizz"          , 82)
             , ("spic"          , 83)
             , ("colLegno"      , 84) ]

  
makeQBass :: String -> String -> (Int,Int) -> (Int,Int) -> (Int,Int) ->
             (Int,Int) -> [(String,QAlias)] -> Meta
makeQBass name staffName mainChan shortChan tubaChan pianoChan aliases =
  MetaQ (Q (qInit_impl qConf) (qRun_impl qConf) staffName
            (mkCommon name [staffName]))
  where
    qConf = QConfig
      { qbKsPitches = bassKSMap
      , qbKsVels    = map ((,velCurveBasic) . fst) bassKSMap
      , qbShortVel  = VelCurve [(0.45,10),(8.55,64)]
      , qbAliases   = aliases
      , qbDestFns   = [ qMakeDest_main  (-12) mainChan
                      , qMakeDest_short (-12) shortChan
                      , qMakeDest_piano (-12) pianoChan
                      , qMakeDest_bass_tuba tubaChan
                      ]
      , qbInits     = [ (mainChan , ( 64,110))
                      , (shortChan, (110,127))
                      , (tubaChan , (100, 90))
                      , (pianoChan, (127,127))
                      ]
      }




qMakeDest_bass_tuba :: (Int,Int) -> Q -> QConfig -> TrNote ->
                       Tr (Maybe DestData)
qMakeDest_bass_tuba chan q qConf note
  | tnNomPitch note > 39 = return Nothing
  | otherwise = do
      loud <- lookupLoud note
      let vel = lookupVel "q tube, " loud $ VelCurve [(0.45,10),(8.55,127)]
      return $ Just $ DestData chan (tnNomPitch note-12) vel []


----------------------------------------------------------------------

qInit_impl :: QConfig -> Q -> Tr Q
qInit_impl qConf q = do
  let x :: ((Int,Int),(Int,Int)) -> [TrRaw]
      x (nums,(vol,expr)) =
        [ TrRaw (qStaffN q) 0 nums 0xB0  7 vol
        , TrRaw (qStaffN q) 0 nums 0xB0 11 expr ]
  includeInitRaws (concatMap x $ qbInits qConf)
  return q


qRun_impl :: QConfig -> Q -> Int -> Int -> Tr ()
qRun_impl qConf q mBeg mEnd = 
  (concat `liftM` forOneStaff nominalPitsTs (MetaQ q) (qStaffN q) mBeg mEnd)
    >>= mapM (runDetails q qConf)
    >>= alterTOff
    >>= includeNotes


runDetails :: Q -> QConfig -> TrNote -> Tr TrNote
runDetails q qConf note = do
  slurs <- (stSlurs . qLookup "0nbw" (tnStaffName note) . scStaves)
           `liftM` gets tsScore
  loud  <- (clipLoud . computeAccent note) `liftM` lookupLoud note
  let alterEnd = utilEndAlter (MetaQ q) slurs note
  dests <- catMaybes `liftM` mapM (\f -> f q qConf note) (qbDestFns qConf)
  return note { tnOnOff = tnOnOff note
              , tnDests = dests
              , tnAlterEnd = alterEnd
              , tnSepSame = 0.1 }


qMakeDest_main :: Int -> (Int,Int) -> Q -> QConfig -> TrNote ->
                  Tr (Maybe DestData)
qMakeDest_main mainPitchShift mainChan q qConf note
  | Staccato `elem` (cModifiers $ tnChord note) = return Nothing
  | otherwise = do
    let scoreAlias = qLookupLE (tnLoc note) $ qLookup "81$6"
                     (tnStaffName note) $ mArtics $ MetaQ q
        alias = case lookup scoreAlias (qbAliases qConf) of
                  Nothing -> throwMine $ "missing alias " ++ scoreAlias
                  Just x  -> x
    loud <- lookupLoud note
    let vel = case alias of
                QaKs ksName -> lookupVel "617g" loud $ qFromJust "k239" $
                               lookup ksName $ qbKsVels qConf
                QaShort -> lookupVel "019b " loud $ qbShortVel qConf
        mods = case alias of
                QaKs ksName ->
                  let p = qFromJust "015f7" $ lookup ksName (qbKsPitches qConf)
                  in [ModifKs (Left $ -0.05) p]
                _ -> []
    return $ Just $
      DestData mainChan (tnNomPitch note + mainPitchShift) vel mods


qFromJust msg x = case x of
  Just y  -> y
  Nothing -> throwMine msg


qMakeDest_short :: Int -> (Int,Int) -> Q -> QConfig -> TrNote ->
                   Tr (Maybe DestData)
qMakeDest_short pitchShift shortChan q qConf note
  | not $ Staccato `elem` (cModifiers $ tnChord note) = return Nothing
  | otherwise = do
      loud <- lookupLoud note
      let vel = lookupVel "q short dest, " loud $ qbShortVel qConf
      return $ Just $ DestData shortChan (tnNomPitch note+pitchShift) vel []


qMakeDest_piano2 _ _ _ _ _ = return Nothing


qMakeDest_piano :: Int -> (Int,Int) -> Q -> QConfig -> TrNote ->
                   Tr (Maybe DestData)
qMakeDest_piano pitchShift chan q qConf note = do
  loud <- lookupLoud note
  let vel = lookupVel "q piano, " loud $ VelCurve [(0.45,10),(8.55,64)]
  return $ Just $ DestData chan (tnNomPitch note+pitchShift) vel []


qLookupLE :: Ord k => k -> Map k a -> a
qLookupLE k m = case M.lookupLE k m of
  Just (_,v) -> v

qLookup :: Ord k => String -> k -> Map k a -> a
qLookup msg k m = case M.lookup k m of
  Just x -> x
  Nothing -> throwMine msg




