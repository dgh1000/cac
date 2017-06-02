{-# LANGUAGE TypeSynonymInstances #-}
module Midi.ShowMidi where

import Text.Printf
import Util.Showable
import Midi.MidiExport
import qualified Midi.MidiData as MiD


instance Showable MidiEvent where
  showi (NoteEvent t on off timeOff modifiers legFlag) =
    Component (printf "NoteEvent-type: on-time: %s  off-time: %s" 
                      (show t) (show timeOff)) True 
      [ SingleLine $ "On: " ++ briefShowRawMidiEvent on
      , SingleLine $ "Off: " ++ briefShowRawMidiEvent off
      , SingleLine $ printf "Time off: %s" (show timeOff)
      , SingleLine $ printf "Legato flag: %s" (show legFlag)
      , Component "Modifiers:" True (map showi modifiers)
      ]
  showi (SingleEvent t its rme) = 
      SingleLine $ printf "Continuous-type: on-time: %s -- %s" itss es
        where
          itss = if its 
                 then "(track start)"
                 else show t
          es = briefShowRawMidiEvent rme

instance Showable RawMidiEvent where
  showi = SingleLine . briefShowRawMidiEvent

briefShowRawMidiEvent :: RawMidiEvent -> String
briefShowRawMidiEvent (RawMidiEvent stream chan status data1 data2) =
  printf "str:%2d chan:%2d status:%3d data1:%3d data2:%3d"
         stream chan status data1 data2

  