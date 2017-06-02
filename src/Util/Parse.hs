module Util.Parse where

import Text.Parsec
import Text.Parsec.String

parseInt :: Parser Int
parseInt = do
  ds <- many1 digit
  return $ read ds


parseInteger :: Parser Integer
parseInteger = do
  ds <- many1 digit
  return $ read ds


parseDouble :: Parser Double
parseDouble = do
  s <- many1 (oneOf "-0123456789.")
  case reads s of
    (x,[]):_ -> return x
    _        -> fail ""


parseFraction :: Parser Double
parseFraction = do
  i1 <- parseInt
  char '/'
  i2 <- parseInt
  return $ fromIntegral i1 / fromIntegral i2


{-
parseRatio :: Parser (Int,Int)
parseRatio = do
  i1 <- parseInt
  char ':'
  i2 <- parseInt
  return (i1,i2)
-}


parseFractional :: Parser Double
parseFractional = try parseFraction <|> try parseDouble
