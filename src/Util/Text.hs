module Util.Text where

indent n s = unlines . map (replicate n ' ' ++) . lines $ s

