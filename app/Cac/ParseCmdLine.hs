module App.Cac.ParseCmdLine where


import Text.Parsec
import Text.Parsec.String
import Data.Functor
import Util.Exception
import App.Cac.AppCacData

{-

(plays specified test phrase with one previous phrase)
p <test name>

(plays specified test phrase with up to three previous phrases)
p <test phrase name> p3 

(plays specified test phrase with 20 previous second)
p <test phrase name> s20

(plays specified test phrase with whole thing)
p <test phrase name> w

(shows report of all available test phrases)
r

(selects filename)
s <fname without .dat extension>

(quits program)
q

-}



parseInt :: Parser Int
parseInt = do
  ds <- many1 digit
  return $ read ds

prevPhrases :: Parser Int
prevPhrases = do
  char 'p'
  parseInt

prevTime :: Parser Int
prevTime = do
  char 's'
  parseInt

parsePrev :: Parser PrevComp
parsePrev = do
  skipMany space
  (do try eof
      return $ PrevPhrases 1)
  <|>
  (do n <- try prevPhrases
      return $ PrevPhrases n)
  <|>
  (do s <- try prevTime
      return $ PrevTime (fromIntegral s))
  <|>
  (do try (char 'w') 
      return PrevWhole)
   

parsePhraseName :: Parser String
parsePhraseName = many1 alphaNum

parseCmdLine :: Parser Cmd
parseCmdLine = do
  try (do char 'p'
          skipMany space
          name <- parsePhraseName
          skipMany space
          prev <- parsePrev
          return $ CmdPlay name prev)
  <|>
  try (do char 'r'
          return CmdReport)
  <|>
  try (do char 's'
          skipMany space
          n <- many1 alphaNum
          return $ CmdSelect (n ++ ".dat"))
  <|>
  try (do char 'q'
          return CmdQuit)

