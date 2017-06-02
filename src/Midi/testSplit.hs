
import Common.CommonExport
import Midi.Split
import Midi.MidiExport

mkRawMidiEvt = RawMidiEvent 0 0 0 0 0

mkMidiNoteEvt loc1 loc2 = 
  NoteEvent (MidiTime 0) loc1 loc2 mkRawMidiEvt mkRawMidiEvt (MidiTime 0)
            [] False

evts = [ mkMidiNoteEvt (Loc 1 1) (Loc 1 4)
       , mkMidiNoteEvt (Loc 1 1) (Loc 1 2)
       , mkMidiNoteEvt (Loc 1 2) (Loc 2 2)
       , mkMidiNoteEvt (Loc 1 3) (Loc 2 1) ]

main = do
  let evts2 = mapEventsToGapData evts
  mapM_ (putStrLn . show) evts2
