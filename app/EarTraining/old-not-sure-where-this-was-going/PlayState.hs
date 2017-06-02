module PlayState(PlayState(..)) where

import Sound.PortMidi
import MusDoc.Data(MusDoc,Loc)

data PlayState = PlayState
                 { verts :: [Loc]
                 , len :: Int
                 , doc :: MusDoc
                 , stream :: PMStream
                 , cursor :: Int  
                 }
