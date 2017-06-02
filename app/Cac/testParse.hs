
import Text.Parsec
import Text.Parsec.String
-- import App.Cac.ParseCmdLine


parseInt :: Parser Int
parseInt = do
  ds <- many1 digit
  return $ read ds


parseRemain :: Parser Int
parseRemain = 
  (do try eof
      return 1)
  <|>
  (do i <- try parseInt
      return i)

parseCmd :: Parser (Int,Int)
parseCmd = do
  skipMany space
  i1 <- parseInt
  skipMany space
  i2 <- parseRemain
  return (i1,i2)
  


main = do
  let s = "3 5"
  case parse parseCmd "" s of
    Left err -> putStrLn (show err)
    Right x -> print x


