{-#  LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module Translation.ShowTranslation where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Map(Map)
import Text.Printf
import Util.Showable
import Translation.TranslationData
import Score.ScoreData
import Common.CommonUtil
import Common.CommonData
import Midi.MidiData
import Midi.ShowMidi


{-


instance ShowItemClass DebugItem where
  showI (DebugNote (ChordKey staffName chordLoc voiceNum chord) 
        case_ timeModel pitch begLoudness) =
    Component (printf "%s %s vn:%d case:%s %s" staffName 
               (simpleShowLoc chordLoc)
               voiceNum (showCase case_) (showMaybeLoudness begLoudness)) True
      [ SingleLine $ showTimeModel timeModel
      , SingleLine $ "pitch: " ++ showPitchRecord pitch
      ]
    where
      showMaybeLoudness Nothing = "Nothing"
      showMaybeLoudness (Just l) = printf "%5.2f" l
      showPitchRecord (TrillTremPitchRecord xs ys) = 
        concatMap (printf "%3d") xs ++ ", " ++ concatMap (printf "%3d") ys
      showPitchRecord (SinglePitchRecord p) = show p
      showCase (SingleCase _) = "Single "
      showCase (TrillCase _)  = "Trill  "
      showCase TremoloCase    = "Tremolo"
  showI (DebugSusPed loc evt name) = SingleLine $ 
    printf "%s: %s %s" (show evt) (simpleShowLoc loc) name

-}


{-

            removing stuff for showing midi events


instance ShowItemClass WrappedMidiEvent where
  showI (WrappedMidiEvent (NoteEvent _ _ noteOn _ _ _ _ _ _ _ _)) =
    SingleLine $ printf "NoteEvent velOn:%d" (rmeData2 noteOn)
  showI (WrappedMidiEvent (SingleEvent t isStart 
          (RawMidiEvent stream chan status data1 data2) sn)) =
    SingleLine $ printf "SingleEvent t:%8.3f status:%2x data1:%2x data2:%2x"
      t status data1 data2


showTimeModel :: TimeModel -> String
showTimeModel (RegularModel nomBeg nomEnd actBeg actEnd _ _) =
  printf "Regular   nomBeg:%8.3f nomEnd:%8.3f actBeg:%8.3f actEnd:%8.3f"
  nomBeg nomEnd actBeg actEnd
showTimeModel (TrillTremModel tBeg tEnd) =
  printf "TrillTrem time: tBeg:%8.3f tEnd:%8.3f" tBeg tEnd

-}


type StaffLoudnessCurve = (String,LoudnessCurve Loc)
instance ShowItemClass StaffLoudnessCurve where
  showI (name,cur) = Component name True (map showI . M.toAscList $ cur)

type StaffLoudnessCurves = (String,[LoudnessCurve Loc])
instance ShowItemClass StaffLoudnessCurves where
  showI (name,curs) = Component name True (map showOneCurve curs)
    where
      showOneCurve :: LoudnessCurve Loc -> ShowItem
      showOneCurve c = Component "LoudnessCurve" True 
                       (map showI . M.toAscList $ c)

type LocDynSeg = (Loc,DynSeg Loc)
instance ShowItemClass LocDynSeg where
  showI (loc,DynSeg level1 level2 end) = SingleLine $ 
    printf "%s %8.1f %8.1f end:%s" (simpleShowLoc loc) level1 level2 
    (simpleShowLoc end)


type StaffLoudnessCurveDs = (String,[LoudnessCurve Double])
instance ShowItemClass StaffLoudnessCurveDs where
  showI (name,curs) = Component name True (map showOneCurve curs)
    where
      showOneCurve :: LoudnessCurve Double -> ShowItem
      showOneCurve c = Component "LoudnessCurveD" True 
                       (map showI . M.toAscList $ c)



type LocDynSegD = (Double,DynSeg Double)
instance ShowItemClass LocDynSegD where
  showI (loc,DynSeg level1 level2 end) = SingleLine $
    printf "time:%8.3f level1:%4.1f level2:%4.1f endTime:%8.3f" loc level1
    level2 end


{-

         removing stuff for showing old TrState


instance ShowItemClass TrState where
  showI (TrState _ _ _ absTimeMaps _ _ loudDebug _) = 
    Component "Computed state" True [sATMs,sLDebugs]
    where
      sATMs    = SingleLine "ATMs not currently shown"
      sLDebugs = Component "LoudnessDebug records" True
                 (map showI . concat $ M.elems loudDebug)

-}


instance ShowItemClass LoudnessDebug where
  showI (LoudnessDebug name curve) = Component name True items
    where
      items = map g $ M.toAscList curve
      g (loc,DynSeg l1 l2 end) = SingleLine $ printf "%s -> %s %10.3f %10.3f"
                                              (showLoc2 loc) (showLoc2 end)
                                              l1 l2


instance ShowItemClass RelTimeMap where
  showI (RelTimeMap m) = Component "" False (map g . M.toAscList $ m)
    where
      g (loc,d) = SingleLine $ 
        printf "%20s %8.3f bpm:%8.3f" (showLoc2 loc) d
          (60 / (fromIntegral slicesPerBeat*d))

{-

          removing stuff for showing old EventsRecord

instance ShowItemClass EventsRecord where
  showI (NoteEventsRecord s evts) = Component ("NoteEventsRecord "++s) True
      (map g evts)
    where
      g (NoteEvent tOn _ _ _ _ _ _ _ _ _ _) = SingleLine (show tOn)
  showI (ModEventsRecord s evts) = Component ("ModEventsRecord " ++s) True
      (map showSingleEvt evts)
  showI (FixedEventsRecord s evts) = Component ("FixedEventsRecord "++s) True
      (map showSingleEvt evts)
  showI (SusPedEventsRecord s evts) =Component ("SusPedEventsRecord "++s) True
      (map showSingleEvt evts)

showSingleEvt (SingleEvent t flag _ _) = SingleLine $
        printf "trackStart:%5s %d" (show flag) t

-}


instance ShowItemClass PatternData where
  showI (PatternData m) = Component "pattern data" True 
                          (map g . M.toAscList $ m)
    where
      g :: (Loc,(Double,Double)) -> ShowItem
      g (loc,(dyn,tempo)) = SingleLine $ printf "%s: dyn:%8.3f tempo:%8.3f"
                            (showLoc2 loc) dyn tempo


----------------------------------------------------------------------
----------------------------------------------------------------------
--                     UnitTimeMod


instance ShowItemClass UnitTimeMod where
  showI (UnitRamp loc1 loc2 r1 r2) = SingleLine $ printf ("UnitRamp " ++
    "%s %s %8.2f %8.2f") (showLoc2 loc1) (showLoc2 loc2) r1 r2
  showI (UnitPause loc r) = SingleLine $ printf "UnitPause %s %s"
    (showLoc2 loc) (showRational r)
  showI (UnitWarp mStr eLocs amt) = SingleLine $ printf ("UnitWarp %s" ++
    " %s %s") (sMStaff mStr) (sELocs eLocs) (showRational amt)
  showI (UnitAbsWarp loc1 loc2 amt) = SingleLine $ printf ("UnitAbsWarp " ++
    "%s %s %s") (showLoc2 loc1) (showLoc2 loc2) (showRational amt)



sMStaff :: Maybe String -> String
sMStaff Nothing = " "
sMStaff (Just s) = "'" ++ s ++ "'"
                               

sELocs :: Either (Loc,Loc) (Loc,Loc,Loc) -> String
sELocs (Left (loc1,loc2)) = showLoc2 loc1 ++ " " ++ showLoc2 loc2
sELocs (Right (loc1,loc2,loc3)) = showLoc2 loc1 ++ " " ++ showLoc2 loc2 ++ " "
  ++ showLoc2 loc3


----------------------------------------------------------------------
----------------------------------------------------------------------
--                 FullDebugInfo


{-


instance ShowItemClass FullDebugInfo where
  showI (FullDebugInfo _ _ tMods) = Component "UnitTimeMods" True
                                    (map showI tMods)


-}

----------------------------------------------------------------------
----------------------------------------------------------------------

instance ShowItemClass ConfigFile where
  showI (ConfigFile mis mTv) =
      Component "ConfigFile" True (sMTv:map showI mis)
    where
      sMTv = case mTv of
        Nothing -> SingleLine "no timing-variation element"
        Just (TimingVariation minL maxL ratio1 ratio2 delta1 delta2) ->
          Component (printf "timing-variation, %d -> %d" minL maxL)
          True [ SingleLine $ printf "ratio1:%8.3f ratio2:%8.3f" ratio1 ratio2
               , SingleLine $ printf "delta1:%8.3f delta2:%8.3f" delta1 delta2
               ]



