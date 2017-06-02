module Util.Maybe where

nullListToNothing :: [a] -> Maybe [a]
nullListToNothing [] = Nothing
nullListToNothing xs = Just xs
