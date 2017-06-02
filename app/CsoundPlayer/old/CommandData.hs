module App.CsoundPlayer.CommandData where

data Command = CommandTimehump String String Rational Float
             | CommandInstrmap String String
               deriving(Show)

