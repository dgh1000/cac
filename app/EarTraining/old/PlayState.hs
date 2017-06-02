module PlayState( PlayState(..)
                , up_cur
                , showIt ) where

import MusDoc.MusDoc 

data PlayState = PlayState
                 { cur :: Int
                 , verts :: [Loc]
                 , len :: Int
                 , doc :: MusDoc
                 }

showIt (PlayState cur verts len doc) = show cur

up_cur :: Int -> PlayState -> PlayState
up_cur i p = p {cur=i}

