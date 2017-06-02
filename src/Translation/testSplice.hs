
import Control.Arrow
import qualified Data.Map as M
import Midi.MidiData
import Translation.Splice
import Util.Showable
import Util.Map


mkNote :: (Integer,Integer,Int) -> MidiEvent
mkNote (t1,t2,pitch) = NoteEvent t1 t2 (mkRaw pitch) (mkRaw pitch) [] False
                                 [] "Foo"
  where
    mkRaw p = RawMidiEvent 1 1 0x90 p 64


notes1 :: [(Integer,Integer,Int)]
notes1 = [ (   0, 1000, 63)
         , (2000, 5000, 64)
         , (2000, 3000, 65)
         , (4000, 5000, 65)
         ]


m :: [(Integer,Integer,Int)] -> IO ()
m = putStrLn . showiToString . Component "Notes" False . 
 map (showi . PrettyMidiEvent) . concat . map snd . M.toAscList . 
 cut 3000 4000 . listToLMap . map ((meTime &&& id) . mkNote)