
import Text.Parsec
import Text.Parsec.String

parseInt :: Parser Int
parseInt = do
  s <- many1 digit
  return (read s)

data Command = Command String
               deriving(Show)


parseCommand2 :: Parser Command
parseCommand2 = do
  (do try (string "bob")
      return $ Command "foobob")
  <|>
  (do try (string "bhenry")
      return $ Command "henryfoo")

parseCommand :: Parser Command
parseCommand = do
  -- skipMany1 space 
  char '['
  string "timewarp"
  s <- many1 (alphaNum <|> space)
  char ']'
  skipMany space
  return $ Command s

parseFile :: Parser [Command]
parseFile = do
  skipMany space
  cs <- many parseCommand2
  manyTill space eof
  return cs

{-
runParse :: String -> Either String Int
runParse s = case parse parseFile "" s of
  Left err -> Left $ show err
  Right x ->  Right x
-}

main :: IO ()
main = do
  buf <- readFile "example.cfg"
  case parse parseFile "" buf of
    Left err -> putStrLn $ show err
    Right cs -> putStrLn (show cs)

