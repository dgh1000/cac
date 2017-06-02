module Instruments.Piano where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Map(Map)
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Instruments.Trill
import Instruments.Curves
import Instruments.AlterTimes
import Score.ScoreData
import Common.CommonData
import Common.CommonUtil
import Util.Math
import Util.Exception

-- okay so we extend only if it's under a slur. 




makePiano :: String -> (Double,Double,Double) -> Map String (Int,Int) ->
             VelCurve -> VelCurve -> Meta
makePiano name (extend,gap,minDur) dests vCurveSingle vCurveTrill
  = MetaPiano
  Piano { pnoName        = name
        , pnoStaffNs     = M.keys dests
        , pnoInit        = pnoInit_implementation
        , pnoRun         = pnoRun_implementation

        , pnoDests       = dests
        , pnoTimeAlter   = (extend,gap,minDur)
        , pnoVCurveSingle= vCurveSingle
        , pnoVCurveTrill = vCurveTrill
          
        , pnoTremShapes  = M.empty
        , pnoTrillShapes = M.empty
        , pnoStacDurs    = M.empty
        , pnoArtics      = M.empty
        , pnoArpDelta    = M.empty
        }
  

pnoInit_implementation :: Piano -> Tr Piano
pnoInit_implementation piano = do
  let mkInitRaw stName =
        TrRaw stName 0 (pLookup stName $ pnoDests piano) 0xB0 7 126
  includeInitRaws $ map mkInitRaw $ pnoStaffNs piano
  return piano
      
{-
pianoInit :: Map String (Int,Int) -> String -> [String] -> Tr ()
pianoInit dests metaName staves  = do
  let lookupDest staffName = case M.lookup staffName dests of
        Nothing -> throwError $ printf ("in pianoInit, was passed a dest "++
          "map that is missing '%s'") staffName
        Just d -> return (staffName,d)

  -- build guaranteed dest map
  guarDests <- M.fromList `liftM` forM staves lookupDest
  
  -- init events
  let mkInitRaw stName =
        TrRaw stName 0 (pLookup stName guarDests) 0xB0 7 126
  includeInitRaws $ map mkInitRaw staves

  -- make mark maps
  score <- gets tsScore
  let mkMarkMaps staffName = do
        staffData staffName..."marks"..."tremShapes" `trSet`
                    VLocMap (mkTremMap staffName score)
        staffData staffName..."marks"..."trillShapes" `trSet`
                    VLocMap (mkTrillMap staffName score)
        staffData staffName..."marks"..."stac" `trSet`
                    VLocMap (mkStacMap staffName score)
  mapM mkMarkMaps staves

  let insertDest (staffName,dest) = do
        staffData staffName ... "dest" `trSet` VDest dest
  mapM insertDest $ M.toList guarDests
    
  -- make arp map (meta-instr mark map)
  metaData metaName..."arp"  `trSet` VMap (mkArpMap staves score)
-}

-- wee need a way to associate , to recall staff names from meta and run will
-- pass them


pnoRun_implementation :: Piano -> Int -> Int -> Tr Piano
pnoRun_implementation piano mBeg mEnd = do
  concat `liftM`
    mapM (runStaff piano mBeg mEnd) (pnoStaffNs piano)
    >>= includeNotes
    >> return piano
  
{-  
pianoRun :: String -> [String] -> Int -> Int -> Tr ()
pianoRun name staffNames begMsr endMsr =
  concat `liftM` mapM (pianoStaff begMsr endMsr) staffNames >>= includeNotes
-}

runStaff :: Piano -> Int -> Int -> String -> Tr [TrNote]
runStaff piano begMsr endMsr staffName =
  (concat
    `liftM` forOneStaff nominalPitsTs (MetaPiano piano) staffName
       begMsr endMsr)
    >>= mapM (runDest piano)
    >>= mapM (runStac piano)
    >>= mapM (runArp piano)
    >>= mapM runLoud_common
    >>= mapM runLoud_accents
    >>= mapM (runVel piano)
    >>= let (ext,gap,minDur)  = pnoTimeAlter piano
        in alterTOff ext gap minDur


runDest :: Piano -> TrNote -> Tr TrNote
runDest piano note =
  return note {tnChanNums = pLookup (tnStaffName note) $ pnoDests piano}


runStac :: Piano -> TrNote -> Tr TrNote
runStac piano note
  | isShort $ tnChord note = do
      let d = case M.lookupLE (tnLoc note) $ pLookup (tnStaffName note)
                     (pnoStacDurs piano) of
                Nothing -> 0.07
                Just (_,x)  -> x
          (t1,_) = headTimes $ tnOnOff note
      return note {tnOnOff = consTimes "stac" t1 (t1+d) (tnOnOff note)}
  | otherwise = return note


runLoud_accents :: TrNote -> Tr TrNote
runLoud_accents note
  | Accent `elem` (cModifiers $ tnChord note) =
      return note {tnLoud = clipLoud (tnLoud note + 1)}
  | Tenuto `elem` (cModifiers $ tnChord note) =
      return note {tnLoud = clipLoud (tnLoud note - 1)}
  | otherwise = return note


clipLoud l | l < 0.5 = 1
           | l > 8.5 = 8
           | otherwise = l

runVel :: Piano -> TrNote -> Tr TrNote
runVel piano note = do
  let (vary,c,loud) = case note of
        n@TrSingle{tnLoud = l} -> (3,pnoVCurveSingle piano,l)
        n@TrTrill {tnLoud = l} -> (6,pnoVCurveTrill  piano,l)
  let x = lookupVel "in runVel in piano, " loud c
      rMin = max (x-vary) 10
      rMax = min (x+vary) 126
  v <- trRandomR (rMin,rMax)
  return note {tnVel = v}


runArp :: Piano -> TrNote -> Tr TrNote
runArp p note
  | Arpeggiate `elem` (cModifiers $ tnChord note) = do
      (up,dn) <- arpCount (pnoStaffNs p) (tnLoc note) (tnPitch note)
      let delay = case M.lookupLE (tnLoc note) (pnoArpDelta p) of
            Nothing -> throwMine $ printf ("no arpeggio delta specified " ++
              "at or before %s") (showLoc2 $ tnLoc note)
            Just (_,d) | d  < 0 -> -d*fromIntegral dn
                       | d >= 0 ->  d*fromIntegral up
          (t1,t2) = headTimes $ tnOnOff note
      return note {tnOnOff = consTimes "arp" (t1+delay) t2 (tnOnOff note)}
  | otherwise = return note

  
pLookup :: Ord k => k -> Map k a -> a
pLookup k m = case M.lookup k m of {Just x -> x}
