
import Text.Parsec
import Text.Parsec.String



p1 :: Parser String
p1 = 
  try p2
  <|>
  try p3
  <?> "p2 or p3"

p2 = string "foo"
p3 = string "food"


s = "foo"

main = putStrLn $ case parse p1 "" s of
  Left e -> show e
  Right v -> show v

