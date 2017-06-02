
module Instruments.InstrumentsUtil where

import Data.List as L
import Common.CommonExport
import Score.ScoreExport
import Score.Access
import Instruments.InstrumentsData

-- compute loudness 
computeAccentLoudCur1 :: String -> Double -> Tr LoudnessCurve
computeAccentLoudCur1 staffName accentSize = do
  score             <- getScoreTr
  let isVoice1      =  (==1) . ckVoiceNum
      isAccented ck =  S.elem Accent mods || S.elem StrongAccent mods
         where mods =  getChordMods ck
      g :: ChordKey -> (Loc,DynSeg)
      g ck = (getChordLoc ck, DynSeg accentSize accentSize $ getChordEnd ck)
  return . M.froList . map g . filter isVoice1 . filter isAccented $ 
    getStaffChordKeys score staffName



      
      
  