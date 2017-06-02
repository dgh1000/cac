{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.Map as M
import Data.Map(Map)
import Util.Showable



class Stringable s where
  showStr :: s -> String


type ShowableTuple a b = (a,[b])


type ShowableLMap a b = Map a [b]


instance (Stringable a, Showable b) => Showable (ShowableTuple a b) where
  showi (a,bs) = Component (showStr a) True (map showi bs)



