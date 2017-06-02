module Util.Set where

import qualified Data.Set as S
import Data.Set(Set)

{-
lookupBracketing :: Ord a => a -> Set a -> (Maybe a,Maybe a)
lookupBracketing x set =
  case S.splitMember x set of
   (lower,False,upper)->(fst `fmap` S.maxView lower,fst `fmap` S.minView upper)
   (lower,True ,upper) ->(x, fst `fmap` S.minView upper)
-}

lookupBracketing :: Ord a => a -> Set a -> (Maybe a,Maybe a)
lookupBracketing x set =
  case S.splitMember x set of
   (lower,False,upper)->(fst `fmap` S.maxView lower,fst `fmap` S.minView upper)
   (lower,True ,upper) ->(Just x, fst `fmap` S.minView upper)
