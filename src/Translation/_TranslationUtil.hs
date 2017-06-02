module Translation.TranslationUtil where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
import Common.CommonData
import Control.Arrow
import Control.Monad.State
import Data.Map(Map)
import Data.Monoid
import Data.Maybe
import Data.List(elemIndex,sort)
import Score.ScoreData
import Midi.MidiData
import Text.Printf
import Translation.TranslationData
import Translation.TimeMap
import Common.CommonUtil
import Util.Map(splitInclude,reduceLMap,lMapToList)
import Util.Exception


-- translation should import instruments, not vice versa

-- timemaps should be in 

----------------------------------------------------------------------
----------------------------------------------------------------------
--                computing single/trill/tremolo case




----------------------------------------------------------------------
----------------------------------------------------------------------
 --                 query functions


{-

      REMOVED TEMPORARILY Jan 30 2017


isShort :: K a => a -> Bool
isShort = (Staccato `elem`) . modifiersK


isArp :: K a => a -> Bool
isArp = (Arpeggiate `elem`) . modifiersK


-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                time computation utilities


tuMapLookup :: Ord k => k -> Map k a -> a
tuMapLookup k m = case M.lookup k m of {Just x -> x}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                 useful maps built from Marks



-- okay so we will now have access to all the Marks (I mean on all staves) and
-- the distinction between global and local is no longer made in the struct
-- that stores them.
--
--   arpDeltas: all arp deltas, on any staff, are global
--
--   stac durs: just local to staff

-- 

----------------------------------------------------------------------
----------------------------------------------------------------------
--     configuration parameters

{-

getParams :: [(ConfigValueType,String)] -> StaffConfig -> [ConfigValue]
getParams expected sc
  | expectedStrings /= presentStrings = throwMine errMsg
  | otherwise = map (getParamValue sc) expected
  where
    expectedStrings = S.fromList . map snd $ expected
    presentStrings  = S.fromList . M.keys . stcParams $ sc
    errMsg = printf "In staff '%s', expected these params and these only: %s"
             (stcName sc)
             (concatMap (\(_,s) -> s ++ " ") expected)


-- Lookup the value of a parameter named 's' in the configuration file (as
-- present in the staff config), while checking to see if the type in the
-- config file matches the expected type 't'
getParamValue :: StaffConfig -> (ConfigValueType,String) -> ConfigValue
getParamValue sc (t,s) = case M.lookup s (stcParams sc) of
  Just p | t == toConfigValueType p  -> p
         | otherwise -> throwMine $ printf ("in staff '%s' config, " ++
             "expected a parameter type of %s for param %s") (stcName sc)
             (show t) s

-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                 modifier predicates




{-

      REMOVED TEMPORARILY Jan. 30, 2017



hasModifier :: ChordModifier -> ChordKey -> Bool
hasModifier m = (m `elem`) . modifiersK


-}


----------------------------------------------------------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
----------------------------------------------------------------------
--                      Access functions

{-
  
getStaffTr :: String -> Tr Staff
getStaffTr n = do
  score <- gets tsScore
  let Just s = M.lookup n (scStaves score)
  return s

-}


