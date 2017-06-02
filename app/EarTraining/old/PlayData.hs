module PlayData(PlayData(..)) where

import Sound.PortMidi
import MusDoc.Data(MusDoc,Loc)

data PlayData = PlayData
                 { verts :: [Loc]
                 , len :: Int
                 , doc :: MusDoc
                 , stream :: PMStream
                 }

-- showIt (PlayData cur verts len doc) = show cur

