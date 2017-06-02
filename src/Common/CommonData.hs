{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,
             DeriveGeneric, DeriveAnyClass, DefaultSignatures #-}
module Common.CommonData where

import qualified Data.Map as M
import GHC.Generics
import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Identity
import System.Random
import Text.Printf
import Data.Map( Map )
import Data.Maybe
import Util.Showable
import Util.Exception

slicesPerBeat = 16 :: Int


class Monad m => RandomState m where
  getGen :: m StdGen
  putGen :: StdGen -> m ()


trRandomR :: (RandomState m, Random a) => (a,a) -> m a
trRandomR (lo,hi) = do
  gen <- getGen
  let (output,gen') = randomR (lo,hi) gen
  putGen gen'
  -- modify (\s -> s {tsRandomGen = gen'})
  return output


trRandomRs :: (RandomState m, Random a) => (a,a) -> m [a]
trRandomRs (lo,hi) = do
  gen <- getGen
  let (g1,g2) = split gen
  putGen g2
  -- modify (\s -> s {tsRandomGen = g2})
  return $ randomRs (lo,hi) g1

-- Loc:  representation of a location within a music document
data Loc = Loc 
    { msrNum :: Int       -- Measure number
    , beat :: Rational -- Beat of the measure 
                       --   (i.e. beat 1 of the measure, beat 1+1/2, etc.)
    }
    deriving (Show,Eq,Ord,NFData,Generic)


instance ShowStringClass Loc where
  showS (Loc msr beat) = printf "[m:%3d, %2d+%7s]" msr beatInt
                         (show beatRemain)
    where
      beatInt :: Int
      beatInt = floor beat
      beatRemain = beat - fromIntegral beatInt


data TimeSig = TimeSig 
    { tsNumer :: Int  -- time sig numerator
    , tsDenom :: Int  -- time sig denominator
    }
                   deriving(Show)

type TimeSigs = Map Int TimeSig

type TempoMap = Map Loc Int

-- midi pitch, step (0 is C, .. 6 is B), alter, octave
data Pitch = Pitch 
             { midiPitch :: Int 
             , step :: Int
             , pAlter :: Int
             , octave :: Int
             } deriving (Eq,Show,NFData,Generic)



instance Ord Pitch where
    compare p1 p2 = compare (midiPitch p1) (midiPitch p2)


octShiftPitch :: Int -> Pitch -> Pitch
octShiftPitch n (Pitch mp s a o) = Pitch (mp+n*12) s a (o+n)


data CValue = CVString String
            | CVInt Int
            | CVDouble Double
            | CVDoubleList [Double]
            | CVMaybeDouble (Maybe Double)
            | CVMap (Map String CValue)
                   deriving(Show,Eq)

{-


data ConfigValueType = CVTString | CVTInt | CVTDouble
                     deriving(Show,Eq)


toConfigValueType :: ConfigValue -> ConfigValueType
toConfigValueType (ConfigValueString _) = CVTString
toConfigValueType (ConfigValueInt    _) = CVTInt
toConfigValueType (ConfigValueDouble _) = CVTDouble

-}


data Dynamic = 
  SimpleDyn 
    { dynLevel :: Int 
        -- Int is pppp = 0, ppp=1, pp=2, p=3, mp=4, mf=5 , f = 6,
        -- ff=7, fff=8, ffff=9
    , dynVoiceNumber :: Int 
    }
  | 
  Fp       
    { dynVoiceNumber :: Int }
  deriving(Show,Eq,Ord)

toDyn :: String -> Int -> Maybe Dynamic
toDyn s i = toDyn' s <*> Just i

toDyn' :: String -> Maybe (Int -> Dynamic)
toDyn' s = case s of
  "sppp"   -> Just $ SimpleDyn 1
  "ppp"    -> Just $ SimpleDyn 1
  "spp"    -> Just $ SimpleDyn 2
  "pp"     -> Just $ SimpleDyn 2
  "p"      -> Just $ SimpleDyn 3
  "sp"     -> Just $ SimpleDyn 3
  "mp"     -> Just $ SimpleDyn 4
  "mf"     -> Just $ SimpleDyn 5
  "f"      -> Just $ SimpleDyn 6
  "sf"     -> Just $ SimpleDyn 6
  "ff"     -> Just $ SimpleDyn 7
  "sff"    -> Just $ SimpleDyn 7
  "fff"    -> Just $ SimpleDyn 8
  "sfff"   -> Just $ SimpleDyn 8
  "ffff"   -> Just $ SimpleDyn 8
  "fp"     -> Just Fp
  _        -> Nothing


data Notehead = NormalHead
              | DiamondHead
                deriving(Eq,Ord,Show,NFData,Generic)

data Symbol = Symbol String Int  -- symbol name, voice number
            deriving(Eq,Show)

data TempoText = TempoTextMetronome Int
               | TempoTextChange TempoChangeType

data TempoChangeType = Rit | Accel
                 deriving(Eq,Show)

data TempoLine = TempoLine TempoChangeType Loc
               deriving(Show)

data PedalEvt = PedalStart | PedalStop | PedalChange
                deriving(Show,Eq)

-- used to recognize tempo words in MusicXML Directions
standardTempoWords = ["rit.","accel."]

-- whether input comes from Sibelius text output, or XML output
data InputFileType = InputTxt | InputXml
               deriving(Eq)


data Elem = Bracketed Int Int String [Elem]
          | Param Int Int String CValue
          | Single Int Int CValue
            deriving(Show)


type Exc = Except String


runExc :: Exc a -> a
runExc m = case runIdentity (runExceptT m) of
  Left msg -> throwMine msg
  Right v -> v


runExcMsg :: String -> Exc a -> a
runExcMsg msg m = runExc $ withExcept (msg ++) $ m


class ElemValueClass a where
  toValue :: Elem -> Maybe a


{-

data KeyValue = KeyValue (String,Value)


instance ShowItemClass KeyValue where
  showI (KeyValue (key,value)) = case showI value of
    SingleLine s -> SingleLine $ printf "%s: %s" key s
    Component t flag values -> Component key flag values
-}

data KeyValue a = KeyValue (String,a)


instance ShowItemClass a => ShowItemClass (KeyValue a) where
  showI (KeyValue (key,value)) = case showI value of
    SingleLine s -> SingleLine $ printf "%s: %s" key s
    Component t flag values -> Component key flag values
  

data LocKeyValue a = LocKeyValue (Loc,a)


instance ShowItemClass a => ShowItemClass (LocKeyValue a) where
  showI (LocKeyValue (key,value)) = case showI value of
    SingleLine s -> SingleLine $ printf "%s: %s" (showLoc2 key) s
    Component t flag values -> Component (showLoc2 key) flag values
    where
      showLoc2 :: Loc -> String  
      showLoc2 (Loc msr beat) = printf "[m:%3d, %2d+%7s]" msr beatInt 
                                (show beatRemain)
        where
          beatInt :: Int
          beatInt = floor beat
          beatRemain = beat - fromIntegral beatInt


    
                     

instance ShowItemClass CValue where
  showI (CVInt x) = SingleLine $ printf "VInt %d" x
  showI (CVString s) = SingleLine $ printf "VString %s" s
  showI (CVMap m) = Component "VMap" True $
                   map (showI . KeyValue) $ M.toAscList m
  showI (CVDouble d) = SingleLine $ printf "VDouble %f" d


instance ElemValueClass Double where
  toValue (Single _ _ (CVDouble d)) = Just d
  toValue (Single _ _ _) = Nothing
  toValue (Param _ _ _ (CVDouble d)) = Just d
  toValue (Param _ _ _ _) = Nothing
  toValue (Bracketed _ _ _ _) = Nothing
  

instance ElemValueClass String where
  toValue (Single _ _ (CVString d)) = Just d
  toValue (Single _ _ _) = Nothing
  toValue (Param _ _ _ (CVString d)) = Just d
  toValue (Param _ _ _ _) = Nothing
  toValue (Bracketed _ _ _ _) = Nothing
  

instance ElemValueClass Int where
  toValue (Single _ _ (CVInt d)) = Just d
  toValue (Single _ _ _) = Nothing
  toValue (Param _ _ _ (CVInt d)) = Just d
  toValue (Param _ _ _ _) = Nothing
  toValue (Bracketed _ _ _ _) = Nothing


instance ElemValueClass (Maybe Double) where
  toValue (Single _ _ (CVMaybeDouble d)) = Just d
  toValue (Single _ _ _) = Nothing
  toValue (Param _ _ _ (CVMaybeDouble d)) = Just d
  toValue (Param _ _ _ _) = Nothing
  toValue (Bracketed _ _ _ _) = Nothing



instance ShowItemClass Elem where
  showI (Bracketed _ _ name elems) = Component name True (map showI elems)
  showI (Param _ _ name v) = SingleLine $ printf "%s: %s" name (show v)
  showI (Single _ _ v) = SingleLine (show v)


simpleShowElem (Bracketed line col type_ _) =
  printf "Bracketed '%s' at line %d col %d" type_ line col
simpleShowElem (Param line col type_ _) =
  printf "Param '%s' at line %d col %d" type_ line col
simpleShowElem (Single line col _) = printf "Single at line %d col %d"
  line col
  

  
----------------------------------------------------------------------

-- MdNote:  time on, time off, stream index, chan, data1, data2
-- MdCtrl:  time, stream index, chan, ctrl #, ctrl value
data MidiData = MdNote Double Double Int Int Int Int 
              | MdCtrl Double Int Int Int Int
