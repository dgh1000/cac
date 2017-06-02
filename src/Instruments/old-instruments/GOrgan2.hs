
module Instruments.GOrgan where

import qualified Data.Map as M
import Control.Monad.State
import Text.Printf
import Data.Maybe
import Data.Map(Map)
import Score.ScoreData
import Translation.TranslationData
import Translation.Dynamics( computeChordLoudCur,toLoudFunc,addLF
                           , standardModifiersToLoudFunc )
import Translation.TranslationUtil( modifyArpTime, modifyStacTime
                                  , isShort,isArp
                                  , hasModifier, getParams, markMapUtils )
import Translation.TimeMap
import Util.Math
import Util.Exception
import Common.CommonData

----------------------------------------------------------------------
--                  piano configuration values
configMinDur         = 50 :: Integer  -- milliseconds
configDefStac        = 0.110
configAccentAmount   = 1.5
configDefArp         = 0.05
configArpMinDur      = 0.05   -- after shifting arp note begin time, will make
                              -- sure has this dur



gOrgan :: Score -> StaffConfig -> Instrument
gOrgan score sc = Instrument
  { iTimeFn           = timeFn stacDurs arpDeltas
  , iVelFn            = velFn
  , iDestFn           = destFn sc techText
  , iMakeLoudnessFn   = makeLoudnessFn staff
  , iTrillShape       = trillShapes
  , iTremShape        = tremShapes
  , iAlterTOff        = TruncExtConfig { tecMinDur = configMinDur
                                       , tecMinSep = 1
                                       , tecSustainedSep = Just 80
                                       , tecExtend = Just 30
                                       , tecLegExtend = Nothing
                                       , tecMaxRatio = 1.5 }
  , iModConfig        = 
    Just ModConfig
      { mcDest        = lookup1 sc
      , mcControllers = [1]
      , mcValueFn     = modValueFn
      , mcSpacing     = 0.05
      , mcLead        = 0.005
      }
  , iSusPedDest       = Nothing
  , iFixedControls    = [(lookup1 sc,(7,vol))]
  }
  where
    (arpDeltas,stacDurs,trillShapes,tremShapes,techText) = 
      markMapUtils score (stcName sc) "arco"
    Just staff = M.lookup (stcName sc) (scStaves score)
    [ConfigValueInt vol] = getParams [(CVTInt,"vol")] sc


modValueFn x = [round $ scaleClip 1.0 x 8.0 10 127]


{-
timeFn :: Map Loc Double -> Map Loc Double -> NoteKey -> 
          Tr (Double,Double,TimeModel)
timeFn stacDurs arpDeltas noteKey
  | isShort noteKey = staccatoTimeModel stacDurs noteKey
  | isArp   noteKey = arpTimeModel configArpMinDur arpDeltas noteKey
  | otherwise       = regularTimeModel noteKey
  where sn = getStaffName noteKey
-}



timeFn :: Map Loc Double -> Map Loc Double -> NoteKey -> Tr TimeModel
timeFn stacDurs arpDeltas noteKey = do
  (nominalBeg,nominalEnd) <- getBegEndTr noteKey
  return  
    (RegularModel nominalBeg nominalEnd nominalBeg nominalEnd Nothing Nothing)
    >>= (if isShort noteKey 
           then modifyStacTime stacDurs noteKey 
           else return)
    >>= (if isArp noteKey 
           then modifyArpTime configArpMinDur arpDeltas noteKey 
           else return)

velFn _ _ _ = return 64


destFn :: StaffConfig -> TranslationCase -> ChordKey -> (Int,Int,Bool)
destFn sc _ _ =
  let (x,y) = lookup1 sc in (x,y,True)


destFn :: StaffConfig -> Map Loc (Either String String) ->
          TranslationCase -> ChordKey -> ([(Int,Int)],Bool)
destFn sc _ chordKey = x
  where
    


-- I guess for instruments that only ever had one destination stream/channel,
-- I was calling it '1' by default. So this needs to change
lookup1 :: StaffConfig -> (Int,Int)
lookup1 sc = case M.lookup "1" (stcMidiDests sc) of
  Just x  -> x
  Nothing -> throwMine $ printf ("config statement for staff '%s' is missing"++
             " a MIDI destination of '1'") (stcName sc)




makeLoudnessFn = standardModifiersToLoudFunc

{-
makeLoudnessFn :: Staff -> AbsTimeMap -> Map Int LoudnessFunc
makeLoudnessFn staff tm = M.fromList . map (\v -> (v, makeVoice v)) $ [1..4]
  where
    makeVoice voiceNum = mkCurve StrongAccent 2 `addLF` mkCurve Accent 1
                         `addLF` mkCurve Tenuto (-1)
      where
        mkCurve mod accentSize = toLoudFunc tm $ computeChordLoudCur
                                 (hasModifier mod) voiceNum accentSize staff
-}


