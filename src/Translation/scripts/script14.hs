
import Text.Parsec
import Text.Parsec.String

f :: Parser Int
f = do
  s <- many1 digit
  notFollowedBy $ oneOf "."
  return $ read s

floatNum :: Parser Double
floatNum = try $ do
  c <- option "" (string "-")
  n1 <- many1 digit
  char '.'
  n2 <- many1 digit
  notFollowedBy $ char '.'
  if null n1 && null n2
     then fail "invalid float number"
     else return . read $ c ++ n1 ++ "." ++ n2

main = case parse floatNum "" "12.30 ]" of
  Left err -> print err
  Right x -> print x


main2 = print (read "-0.0" :: Double)

