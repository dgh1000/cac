
import Text.Parsec.String
import Text.Parsec


data Foo = Foo { bob :: Int }
         deriving(Show,Read)


main = print $ Foo 3


main2 = print (read "Foo {bob=3}" :: Foo)


floatNum :: Parser Double
floatNum = do
  s <- many1 (oneOf "-0123456789.") 
  case reads s of
    (f,_):_ -> return f
    _       -> fail "invalid float number"


manyFloat = many $ do {f <- floatNum; skipMany space; return f}


test1String = "0.0 1.0 ..]"


test1 = print $ parse manyFloat "" test1String
