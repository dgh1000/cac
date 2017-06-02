{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
module Midi.ShowMidi where

import Text.Printf
import Util.Showable
import Midi.MidiData
import Common.CommonUtil

-- NoteEvent (0,1) p: 61 v:101 leg:[Just 0.2] sepSame:[Nothing]
--     "trim"      8.00   9.11
--     "nominal"   8.00   9.00
--  
--

instance ShowItemClass MidiEvent where
  showI (RawEvent raw) = showI raw
  showI (NoteEvent (stream,chan) onOff mLeg mSepSame _ pit vel _) =
    Component msg1 True $ msg2:sOnOff
    where
      msg1  = printf "NoteEvent  (%d,%d) p:%3d v:%3d" stream chan pit vel 
      sOne :: (String,(Double,Double)) -> String
      sOne (s,(t1,t2)) = printf "%15s %8.3f %8.3f" s t1 t2
      sOnOff = let (OnOff list) = onOff in map (SingleLine . sOne) list
      msg2 = SingleLine $ printf "legato:%s sep same:%s pit:%2d"
             (showMDoub mLeg) (showMDoub mSepSame) pit
  showI (TrillTremEvent (stream,chan) onOff mLeg mSep _ list _) =
    Component (printf "Trill/trem d:(%d,%d) t:%8.3f -%8.3f" stream chan
                      (onTime onOff) (offTime onOff)) True (map showL list)
    where
      showL :: ((Int,Int),OnOff) -> ShowItem
      showL ((pit,vel),o) = SingleLine $ printf "p:%3d v:%3d t%8.3f -%8.3f"
                                         pit vel (onTime o) (offTime o)
      {-
      g :: Int -> String
      g = (++ " ") . show
      sPits = SingleLine $ printf "ps: %s, %s" (concatMap g pits1)
                           (concatMap g pits2)
      h :: OnOff -> Int -> ShowItem
      h o v = SingleLine $ printf "%8.2f %8.2f %3d" (onTime o) (offTime o) v
      sIndv = Component "times, vels" True (zipWith h onOffList vels)
      -}

      
showMDoub :: Maybe Double -> String
showMDoub Nothing = "       "
showMDoub (Just x) = printf "%7.3f" x


instance ShowItemClass RawMidiEvent where
  showI (MidiSingle t stream chan status data1 data2) =
    SingleLine $ printf ("MidiSingle %8.2f s:%2d c:%2d status:%02x " ++
                 "data1:%3d data2:%3d") t stream chan status data1 data2
  showI (MidiPair on off stream chan pitch vel) =
    SingleLine $ printf ("MidiPair   %8.2f %8.2f s:%2d c:%2d pit:%3d " ++
                 "vel:%3d") on off stream chan pitch vel



type Short = (Integer,(Int,Int,Int,Int))

instance ShowItemClass Short where
  showI (t,(stream,status,data1,data2)) =
    SingleLine $
    printf "t:%6d stream:%2d status:%2x data1:%2x data2:%2x"
                 t
                 stream status data1 data2
        

{-



               12/2016: removing stuff for showing old midi stuff



instance ShowItemClass MidiEvent where
  showI (NoteEvent t timeOff on off modifiers _ legFlag _ sn _ _) =
    Component 
      (printf "NoteEvent: %s t:%10d %10d" sn t timeOff) True 
      [ SingleLine $ "On:  " ++ briefShowRawMidiEvent on
      , SingleLine $ "Off: " ++ briefShowRawMidiEvent off
      , Component "Modifiers:" True (map showI modifiers)
      ]
  showI (SingleEvent t trackStart rme _) = 
    Component (printf "Single: t:%8.3f start? %5s" t (show trackStart)) True
      [ SingleLine . briefShowRawMidiEvent $ rme ]


instance ShowItemClass RawMidiEvent where
  showI = SingleLine . briefShowRawMidiEvent

briefShowRawMidiEvent :: RawMidiEvent -> String
briefShowRawMidiEvent (RawMidiEvent stream chan status data1 data2) =
  printf "str:%2d chan:%2d status:%3x data1:%3d data2:%3d"
         stream chan status data1 data2

instance ShowItemClass PrettyMidiEvent where
  showI (PrettyMidiEvent (NoteEvent t timeOff on off modifiers _ legFlag 
                          tHistory staffName _ _)) =
    Component (printf "NoteEvent: %s " staffName ) True
      ( [ SingleLine $ printf "  tOn:%8.3f tOff:%8.3f  pitch:%d vel:%d" 
                      (toSecs t) (toSecs timeOff) (rmeData1 on) (rmeData2 on)
        ]
        -- we want the newest time in tHistory to occur first in the display
        -- so it is right below the absolute newest times, the current
        -- times
        ++ map gHistory (reverse tHistory)
      )
    where
      toSecs :: Integer -> Double
      toSecs x = fromIntegral x / 1000
      gHistory (b,e) = SingleLine $ printf "  tOn:%8.3f tOff:%8.3f" 
                                    (toSecs b) (toSecs e)


-}
