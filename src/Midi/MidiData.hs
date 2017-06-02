{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Midi.MidiData where

import GHC.Generics hiding(Meta)
import Control.DeepSeq


data Short = Short Double Int Int Int Int
             deriving (NFData,Generic)


mkCtrl :: Double -> (Int,Int) -> Int -> Int -> Short
mkCtrl t (str,chan) ctrlNum value = Short t str (0xB0+chan-1) ctrlNum value


