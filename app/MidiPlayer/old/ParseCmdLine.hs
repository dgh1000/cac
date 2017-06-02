module App.MidiPlayer.ParseCmdLine where

{-

okay what are the commands

-- Number by itself (N) means start at measure N and continue to first
-- M blank msrs (where M is specified in the config file)
8

-- Two numbers with a dash means play from first to last (where last is 
-- inclusive). So this means play 8, 9, and 10
8-10

-- You can have multiple msr groups. They must be in ascending order,
-- and only the last can be a single number. They will be joined, so
-- this will play msrs 8, 9, 12, then 15 immediately following 12 in time
-- and continuing to a blank msr(s) 
8-12 15

-- q means quit
q

-- wd<N> by itself will change to one of the hard-coded
--        current-working directories (see RunFunctions.hs for hard-coded 
--        values)
wd2

-- t by itself means terminate the running midi-output thread
t

-}


import Control.Exception hiding(try)
import Data.Functor
import Data.Ratio
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import MusDoc.MusDocData( Loc(..) )
import App.MidiPlayer.MidiPlayerData( PlayArg(..) 
                                    , InpCmd(..)
                                    , MsrRange(..) )
import Util.Exception

..get rid of this module...

parseInt :: Parser Int
parseInt = do
  ds <- many1 digit
  return $ read ds

floatNum :: Parser Float
floatNum = do
  s <- many1 (oneOf "-0123456789.") 
  return $ read s

parseFileName :: Parser String
parseFileName = many1 (alphaNum <|> char '.')

parseArg :: Parser PlayArg
parseArg = do 
  skipMany space
  a <- (do r <- parseMsrRange
           return $ PlayArgMsrRange r)
       <|>
       (do char 'q'
           return PlayArgQuit)
       <|>
       (do char 't'
           return PlayArgTerminateProcess)
       <|>
       (do try (string "wd")
           i <- parseInt
           return $ PlayArgCwd i)
  return a

parseMsrRange :: Parser MsrRange
parseMsrRange = do
  i1 <- parseInt
  i2 <- (do char '-'
            Just <$> parseInt)
        <|>
        return Nothing
  return (i1,i2)

{-
isCsdOnly (PlayArgCsdOnly _) = True
isCsdOnly _ = False
-}

computePlayCmd :: [PlayArg] -> Maybe InpCmd
computePlayCmd as
  | nMsrRangeTypeArgs == 0 = Nothing
  -- | nCsdOnly+nMsrRangeTypeArgs < length as || nCsdOnly > 1 = Nothing
  | otherwise = Just $ InpPlay msrRangeTypeArgs {- csdArgData tempoRatio -}
  where
    msrRangeTypeArgs = [n | PlayArgMsrRange n <- as]
    nMsrRangeTypeArgs = length msrRangeTypeArgs
    -- csdArgData = case [fn | PlayArgCsdOnly fn <- as] of
    --   [] -> Nothing
    --  s:_ -> Just s
    -- tempoRatio = listToMaybe [r | PlayArgTempoRatio r <- as]

parseCmdLine :: Parser InpCmd
parseCmdLine = do
  as <- many parseArg
  case as of
    [PlayArgTerminateProcess] -> return InpTerminateProcess
    -- [PlayArgDumpMusDoc] -> return InpDumpMusDoc
    [PlayArgQuit] -> return InpQuit
    {- [PlayArgDumpPerfDoc] -> return InpDumpPerfDoc -}
    (PlayArgCwd i) : _ -> return $ InpCwd i
    ns ->
      case computePlayCmd ns of
        Just c -> return c
        Nothing -> throwMine "Invalid command line"

