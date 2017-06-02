{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Instruments.ShowInstruments where

import qualified Data.Map as M
import qualified Data.List as L
import Text.Printf
import Util.Showable
import Common.CommonData
import Common.CommonUtil
import Score.ScoreData
import Instruments.InstrumentsData
import Instruments.TimeMap

{-

instance ShowItemClass Value where
  showI (VDouble d) = SingleLine $ printf "VDouble %8.3f" d
  showI (VInt i) = SingleLine $ printf "VInt %d" i
  showI (VString s) = SingleLine $ printf "VString '%s'" s
  showI (VDest (str,chan)) = SingleLine $ printf "SDest (%d,%d)" str chan
  showI (VMap m) = Component "VMap" True $
                   map (showI . KeyValue) $ M.toAscList m
  showI (VLocMap m) = Component "VMap" True $
                      map (showI . LocKeyValue) $ M.toAscList m
  showI (VCurve s) = case s of
    Curve cs -> Component "VCurve" True (map showCurve cs)
  showI (VTrillShape _) = SingleLine "STrillShape (not shown)"

-}


instance ShowItemClass Curve where
  showI (Curve cs) = Component "Curve" True (map showCurve cs)


showCurve :: OneCurve -> ShowItem
showCurve (OneCurve segMap timeSigs tm) =
  Component "OneCurve" True (map (SingleLine . showSeg) $ M.toAscList segMap)
  where
    showSeg (x1,Seg x2 y1 y2) =
      printf "%20s%20s%8.3f%8.3f" (showLoc2 l1) (showLoc2 l2) y1 y2
      where
        l1 = timeToLoc x1 timeSigs tm
        l2 = timeToLoc x2 timeSigs tm

isShort (Chord _ mods _) = Staccato `elem` mods

instance ShowItemClass TrNote where
  showI (TrSingle staffName loc endLoc vn ch note oo dests _
         loud alterEnd sepSame)
    = Component (printf "staffName:%s %s -> %s isShort:%s" staffName
                (showLoc2 loc) (showLoc2 endLoc) (show $ isShort ch)) True
        [ showI oo
        , SingleLine $ printf "alterEnd:%s, sepSame:%s" (show alterEnd)
                       (show sepSame)
        , Component "dests" True $ map showI dests
        ]
  showI (TrTrill staffName loc endLoc vn ch idx count oo dests _ 
         loud alterEnd sepSame)
    = Component (printf "(trill) staffName:%s %s -> %s" staffName
                (showLoc2 loc) (showLoc2 endLoc)) True
        [ showI oo
        , SingleLine $ printf "trill: %d/%d" idx count
        , SingleLine $ printf "alterEnd:%s, sepSame:%s" (show alterEnd)
                       (show sepSame)
        , Component "dests" True $ map showI dests
        ]


instance ShowItemClass DestData where
  showI (DestData (stream, chan) pitch vel mods)
    = Component (printf "(%2d,%2d) pit:%2d vel:%3d" stream chan pitch vel) True
                (map showI mods)


instance ShowItemClass TrRaw where
  showI (TrRaw staffName time (stream,chan) status data1 data2) =
    SingleLine $ printf "TrRaw (%2d,%2d) %2x %3d %3d" stream chan status data1
                        data2

instance ShowItemClass Modif where
  showI (ModifKs timing ks) = SingleLine $ "ModifKs: " ++ show ks
           

instance ShowItemClass OnOff where
  showI (OnOff x) = ooHelp x


{-
type StaffName_TimeMods = (String,[UnitTimeMod])
instance ShowItemClass StaffName_TimeMods where
  showI (staffN,mods) = Component staffN True (map showI mods)
-}

instance ShowItemClass UnitTimeMod where
  showI (UnitWarp mStaffName (Left (loc1,loc2)) amt) =
    SingleLine $ printf "%s Left (%s,%s)" (show mStaffName) (showLoc2 loc1)
                 (showLoc2 loc2)
  showI (UnitWarp mStaffName (Right (loc1,loc2,loc3)) amt) =
    SingleLine $ printf "%s Right (%s,%s,%s)" (show mStaffName)
                 (showLoc2 loc1) (showLoc2 loc2) (showLoc2 loc3)
  showI (UnitAdjust mStaffName _ _ _ _) =
    SingleLine $ printf "UnitAdjust %s" (show mStaffName)


ooHelp [] = SingleLine "empty OnOff"
ooHelp [(msg,(on,off))] = SingleLine $ printf "%s: %8.3f %8.3f" msg on off
ooHelp mult = Component "on/off" True (map doOne mult)
  where
    doOne (msg,(on,off)) = SingleLine $ printf "%20s--%8.3f %8.3f" msg on off
