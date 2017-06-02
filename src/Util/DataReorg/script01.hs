
import Text.Parsec
import Text.Parsec.String

p :: Parser ()
p = do
  skipMany (notFollowedBy adobe >> anyChar)
  adobe
  return ()


adobe = try $ string "Adobe"

main = case parse p "" "xxxAdobexx" of
  Left err -> print err
  Right _  -> print "Yay!"
