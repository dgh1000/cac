module Util.MiscCsound.App.AnalyzeSamples.ParseConfig where

import Text.Parsec
import Text.Parsec.String
import Util.MiscCsound.App.AnalyzeSamples.AnalyzeSamplesData( Foo(..) )

parseBracketed = do
  char '['
  many (noneOf

parseConfigA :: Parser Foo
parseConfigA = return (Foo 1)

