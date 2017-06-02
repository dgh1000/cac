
module Instruments.AlterTimes where

import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import Data.Set(Set)
import Instruments.InstrumentsData
import Instruments.InstrUtils

{-
alterTOff :: Double -> Double -> Double -> [TrNote] -> Tr [TrNote]
alterTOff ext gap minDur notes = do
  let fn note = (tnPitch note,[onTime $ tnOnOff note])
      begSet = M.map S.fromList $ M.fromListWith (++) $ map fn notes
  mapM (alterOne ext gap minDur begSet) notes
-}  

alterTOff :: [TrNote] -> Tr [TrNote]
alterTOff notes = do
  let fn note = (tnNomPitch note,[onTime $ tnOnOff note])
      begSet = M.map S.fromList $ M.fromListWith (++) $ map fn notes
  return $ map (alterOne begSet) notes
  

{-

-- take one note and follow rules to adjust end time. adjustments can include
--
-- extend by a fixed amount
--
-- truncate, if needed from following same pitch by a certain amount
--
-- extend again to minimum dur
alterOne :: Double -> Double -> Double -> Map Int (Set Double) -> TrNote ->
            Tr TrNote
alterOne ext gap minDur begSet note = do
  let extFn oo
        | isShort (tnChord note) = oo
        | otherwise = consTimes (printf "extend %.3f" ext) t1 (t2+ext) oo
        where
          (t1,t2) = headTimes oo
      gapFn oo = case M.lookup (tnPitch note) begSet of
          Nothing -> oo
          Just s -> case S.lookupGT t1 s of
            Nothing -> oo
            Just x
              | x >= t2+gap -> oo
              | otherwise -> consTimes (printf "gap %.3f" gap) t1 (x-gap) oo
        where
          (t1,t2) = headTimes oo
      minDurFn oo | t2-t1 >= minDur = oo
                  | otherwise = consTimes "minDur" t1 (t1+minDur) oo
        where
          (t1,t2) = headTimes oo
  return $ note {tnOnOff = minDurFn . gapFn . extFn $ tnOnOff note}
-}

alterOne :: Map Int (Set Double) -> TrNote -> TrNote
alterOne begSet note =
    note {tnOnOff = minDurFn . gapFunc . endFn $ tnOnOff note}
  where
      alterEnd = tnAlterEnd note
      gap = tnSepSame note
      endFn oo
        | alterEnd >  0 = consTimes "extend" t1 (t2+alterEnd) oo
        | alterEnd <  0 = consTimes "trunc"  t1 (t2+alterEnd) oo
        | otherwise     = oo
        where
          (t1,t2) = headTimes oo
      gapFunc oo = case M.lookup (tnNomPitch note) begSet of
          Nothing -> oo
          Just s -> case S.lookupGT t1 s of
            Nothing -> oo
            Just x
              | x >= t2+gap -> oo
              | otherwise -> consTimes "sepSame" t1 (x-gap) oo
        where
          (t1,t2) = headTimes oo
      minDur = 0.05
      minDurFn oo | t2-t1 >= minDur = oo
                  | otherwise = consTimes "minDur" t1 (t1+minDur) oo
        where
          (t1,t2) = headTimes oo
   
