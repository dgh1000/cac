
import Control.Arrow
import qualified Data.Map as M
import Midi.MidiData
import Midi.ShowMidi
import Translation.AlterTOff
import Translation.TranslationData
import Util.Showable
import Util.Map



mkNote :: (Integer,Integer,Int,Int,Bool,Bool,Bool) -> MidiEvent
mkNote (t1,t2,pitch,chan,legato,sustained,short) = 
    NoteEvent t1 t2 (mkRaw chan pitch) (mkRaw chan pitch) [] 
              legato [] "Foo" sustained short
  where
    mkRaw chan p = RawMidiEvent 1 chan 0x90 p 64


-- notes1 :: [(tOn,tOff,pitch,channel,legato,sustained,short)]
notes1 :: [(Integer,Integer,Int,Int,Bool,Bool,Bool)]
notes1 = [ (   0,  100, 63, 1, False, True,  False)
         , ( 100, 5000, 63, 1, False, False, True)
         ]



tec1 = TruncExtConfig { tecMinDur       = 50
                      , tecMinSep       = 1
                      , tecSustainedSep = Just 50
                      , tecExtend       = Just 100
                      , tecLegExtend    = Just 10
                      , tecMaxRatio     = 1.5 }


m tec notes = putStrLn . showiToString . Component "Notes" False .
  map (showi . PrettyMidiEvent) .
  concat . map snd . M.toAscList .
  alterTOff tec .
  listToLMap . map ((meTime &&& id) . mkNote) $ notes