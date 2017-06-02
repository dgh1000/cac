
import Util.ParseStuff
import Text.Parsec
import qualified Data.ByteString as B

{-
t1 = [ ("foo",SomeDataTypeString)
     , ("likeWow", SomeDataTypeInt)]

t2 = [("groupName1",t1)]
-}

main = do
  s <- B.readFile "parseTest.txt"
  case parse parseSquareBracketed "" s of
    Left err -> putStrLn (show err)
    Right s -> putStrLn (show s)