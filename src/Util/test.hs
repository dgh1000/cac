module Util.Env where

import System.Environment

defaultMidiDevice :: IO Int
defaultMidiDevice = do
  envs <- getEnvironment
  return $ case lookup "DEFAULT_MIDI_OUT" envs of
             Just n -> case reads n of
                        []      -> 11
                        (i,_):_ -> i
             Nothing -> 11

algoPath :: IO String
algoPath = do
  envs <- getEnvironment
  return $ case lookup "ALGO" envs of
             Just s -> s
             Nothing -> error "environment variable ALGO not defined"
