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




makePiano :: String -> Map String (Int,Int) ->
             VelCurve -> VelCurve -> Meta
makePiano name dests vCurveSingle vCurveTrill
  = MetaPiano
  Piano { -- pnoName        = name
        -- , pnoStaffNs     = M.keys dests
          pnoInit         = pnoInit_implementation
        , pnoRun          = pnoRun_implementation

        , pnoDests        = dests
        , pnoVCurveSingle = vCurveSingle
        , pnoVCurveTrill  = vCurveTrill

        , pnoCommon = MetaCommon { mcName        = name
                                 , mcStaffNs     = M.keys dests
                                 , mcTremShapes  = M.empty
                                 , mcTrillShapes = M.empty
                                 , mcStacDurs    = M.empty
                                 , mcArtics      = M.empty
                                 , mcArpDelta    = M.empty
                                 , mcLegExt      = M.empty
                                 , mcTrunc       = M.empty
                                 , mcCurves      = M.empty
                                 }
        }
  

pnoInit_implementation :: Piano -> Tr Piano
pnoInit_implementation piano = do
  let mkInitRaw stName =
        TrRaw stName 0 (pLookup stName $ pnoDests piano) 0xB0 7 126
  includeInitRaws $ map mkInitRaw $ mStaffNs $ MetaPiano piano
  return piano


pnoRun_implementation :: Piano -> Int -> Int -> Tr ()
pnoRun_implementation piano mBeg mEnd = do
  concat `liftM`
    mapM (runStaff piano mBeg mEnd) (mStaffNs $ MetaPiano piano)
    >>= includeNotes
  

runStaff :: Piano -> Int -> Int -> String -> Tr [TrNote]
runStaff piano begMsr endMsr staffName =
  (concat
    `liftM` forOneStaff nominalPitsTs (MetaPiano piano) staffName
       begMsr endMsr)
    >>= mapM (runPianoDetails piano)
    >>= alterTOff
  


-- It is assumed that nominal time and pitch has been run on the note before
-- calling this.
runPianoDetails :: Piano -> TrNote -> Tr TrNote
runPianoDetails piano note = do
  slurs <- (stSlurs . pLookup (tnStaffName note) . scStaves)
              `liftM` gets tsScore
  let chans = pLookup (tnStaffName note) $ pnoDests piano
  ooStacArp <- timingArp piano note $ timingStac piano note $ (tnOnOff note)
  loud <- (clipLoud . computeAccent note) `liftM` lookupLoud note
  vel  <- computeVel piano note loud
  let alterEnd = utilEndAlter (MetaPiano piano) slurs note
  {-
  let isStac = Staccato `elem` (cModifiers $ tnChord note)
      isSlur = case M.lookupLE (tnLoc note) slurs of
        Nothing -> False
        Just (_,endSlur) | tnLoc note < endSlur -> True
                         | otherwise            -> False
      trunc = case M.lookupLE (tnLoc note)
                   $ pLookup (tnStaffName note) (pnoTrunc piano) of
        Nothing -> throwMine "needs a Trc score mark"
        Just (_,x) -> x
      ext = case M.lookupLE (tnLoc note)
                 $ pLookup (tnStaffName note) (pnoLegExt piano) of
        Nothing -> throwMine "needs a Leg score mark"
        Just (_,x) -> x
      alterEnd = case (isStac,isSlur) of
        (True,_)      -> 0
        (False,True)  -> ext
        (False,False) -> -trunc
  -}
  let destData = DestData chans (tnNomPitch note)  vel []
  return note { tnOnOff = ooStacArp, tnDests = [destData]
              , tnAlterEnd = alterEnd
              , tnSepSame = 0.002 }
  

timingStac piano note oo
  | isShort $ tnChord note =
      let d = case M.lookupLE (tnLoc note) $ pLookup (tnStaffName note)
                   (mStacDurs $ MetaPiano piano) of
                Nothing -> 0.07
                Just (_,x)  -> x
          (t1,_) = headTimes oo
      in consTimes "stac" t1 (t1+d) oo
  | otherwise = oo
          

timingArp :: Piano -> TrNote -> OnOff -> Tr OnOff
timingArp p note oo
  | Arpeggiate `elem` (cModifiers $ tnChord note) = do
      (up,dn) <- arpCount (mStaffNs $ MetaPiano p) (tnLoc note)
                          (tnNomPitch note)
      let delay = case M.lookupLE (tnLoc note) (mArpDelta $ MetaPiano p) of
            Nothing -> throwMine $ printf ("no arpeggio delta specified " ++
              "at or before %s") (showLoc2 $ tnLoc note)
            Just (_,d) | d  < 0 -> -d*fromIntegral dn
                       | d >= 0 ->  d*fromIntegral up
          (t1,t2) = headTimes $ tnOnOff note
      return $ consTimes "arp" (t1+delay) t2 (tnOnOff note)
  | otherwise = return oo







computeVel :: Piano -> TrNote -> Double -> Tr Int
computeVel piano note loud = do
  let (vary,c) = case note of
        TrSingle{} -> (3,pnoVCurveSingle piano)
        TrTrill {} -> (6,pnoVCurveTrill  piano)
  let x = lookupVel "in runVel in piano, " loud c
      rMin = max (x-vary) 10
      rMax = min (x+vary) 126
  trRandomR (rMin,rMax)


pLookup :: Ord k => k -> Map k a -> a
pLookup k m = case M.lookup k m of {Just x -> x}
