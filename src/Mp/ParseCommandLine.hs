
{-

Example command lines:

  10

    start playback at measure 10 and continue until three blank measures

  10-13

    start playback at measure 10 and continue through end of measure 13

  10 b

    start playback at measure 10, continue until the first occurrence of a
    splice mark $a, then jump from $a to $b and continue until either the next
    splice mark or until three blank measures

  10 a

    start playback at measure 10, continue until $a, then keep going until
    next splice mark or three blank measures



-}


module Mp.ParseCommandLine where

import Text.Parsec
import Text.Parsec.String
import Mp.MpData
import Util.Exception

parseInt :: Parser Int
parseInt = do
  ds <- many1 digit
  return $ read ds


floatNum :: Parser Double
floatNum = do
  s <- many1 (oneOf "-0123456789.") 
  case reads s of
    (x,_):_ -> return x
    _  -> throwMine "Can't read float number"

parseFileName :: Parser String
parseFileName = many1 (alphaNum <|> char '.')

{-
parseArg :: Parser PlayArg
parseArg = do 
  skipMany space
  a <- (do (r,s) <- parseMsrRange
           return $ PlayArgMsrRange r s)
       <|>
       (do try (char 'q')
           return PlayArgQuit)
       <|>
       (do try (char 's')
           return PlayArgTerminateProcess)
       <|>
       (do try (string "wd")
           PlayArgCwd <$> parseInt)
       <|>
       (do try (string "list")
           return PlayArgShowDirs)
       <|>
       (do try (string "ct")
           PlayArgChangeTempo <$> floatNum)
  skipMany space
  return a
-}

transpose = string "tr" >> (InpTranspose <$> parseInt)


changeTempo = string "ct" >> (InpChangeTempo <$> floatNum)


showDirs = string "list" >> return InpShowDirs


cwd = do
  string "wd"
  InpCwd <$> parseInt


term = char 's' >> eof >> return InpTerminateProcess


quit = char 'q' >> eof >> return InpQuit


play :: Parser InpCmd
play = do
  b1 <- parseInt
  b2 <- (do char '-'
            i <- parseInt
            many space
            return $ Just i)
        <|>
        return Nothing
  many space
  b3 <- optionMaybe $ oneOf ['a'..'w']
  many space
  eof
  return $ InpPlay (b1,b2) b3


parseCmdLine :: Parser InpCmd
parseCmdLine = 
  try play <|> try quit <|> try term <|> try cwd <|> try showDirs <|> 
  try changeTempo <|> try transpose

{-
parseCmdLine = do
  args <- many parseArg
  return (case args of
    [PlayArgChangeTempo r]    -> InpChangeTempo r
    [PlayArgTerminateProcess] -> InpTerminateProcess
    [PlayArgQuit]             -> InpQuit
    [PlayArgCwd i]            -> InpCwd i
    [PlayArgMsrRange r s]     -> InpPlay r s
    [PlayArgShowDirs]         -> InpShowDirs
    _                         -> throwMine "unrecognized command line")
-}
