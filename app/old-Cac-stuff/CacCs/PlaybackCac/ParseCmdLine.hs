{-

Command line args

-- quit
q

-- stop running process
s

-- marker names with dash between means play music within that range of markers
--
-- or one marker name with dash before or after means play all the way to/from
--   that marker
<marker1>-<marker2>
<marker1>-
-<marker2>
-                             :: dash by itself play whole composition

-- example of above
  sectiont1A-section2B




-}

module PlaybackCac.ParseCmdLine where

import Text.Parsec
import Text.Parsec.String
import Util.Exception
import PlaybackCac.PlaybackCacData

parseMarker :: Parser String
parseMarker = many1 alphaNum

parseEndBoundary :: Parser ()
parseEndBoundary = lookAhead (((char ' ') >> return ())  <|> eof)


playArgPlayCase1 :: Parser PlayArg
playArgPlayCase1 = do
  char '-'
  parseEndBoundary
  return $ PlayArgPlay Nothing Nothing

playArgPlayCase2 :: Parser PlayArg
playArgPlayCase2 = do
  char '-'
  m <- parseMarker
  return $ PlayArgPlay Nothing (Just m)

playArgPlayCase3 :: Parser PlayArg
playArgPlayCase3 = do
  m1 <- parseMarker
  char '-'
  m2 <- parseMarker
  return $ PlayArgPlay (Just m1) (Just m2)

playArgPlayCase4 :: Parser PlayArg
playArgPlayCase4 = do
  m1 <- parseMarker
  char '-'
  return $ PlayArgPlay (Just m1) Nothing


parsePlayArgPlay :: Parser PlayArg
parsePlayArgPlay =
  try playArgPlayCase1
  <|>
  try playArgPlayCase2
  <|>
  try playArgPlayCase3
  <|>
  try playArgPlayCase4

parseArg :: Parser PlayArg
parseArg = do
  a <- try (do char 'q'
               parseEndBoundary
               return PlayArgQuit)
       <|> 
       try parsePlayArgPlay
       <|>
       try (do char 's'
               parseEndBoundary
               return PlayArgStop)
  return a
  
parseCmdLineHelp :: Parser [PlayArg]
parseCmdLineHelp = sepEndBy parseArg (many1 space)

parseCmdLine :: Parser InpCmd
parseCmdLine = do
  as <- parseCmdLineHelp
  case as of
    [PlayArgPlay s1 s2] -> return $ InpPlay $ PlayParams s1 s2
    [PlayArgQuit] -> return InpQuit
    [PlayArgStop] -> return InpTerminateProcess
    _ -> throwMine "Invalid command line"

{-
parseTest2 :: Parser String
parseTest2 = 
  (do char '-'
      s <- many1 alphaNum
      return s)
  <|>
  (do parseEndBoundary
      return "foo")
  {-
  many1 alphaNum
  (space >> return ()) <|> eof
  -}

test3 = do
  let s = "y87- s"
  case parse parseCmdLineHelp "" s of
     Left err -> print err
     Right c -> print c

test2 = do
  case parse parseTest2 "" "foo " of
    Left err -> print err
    Right c -> print c
-}
