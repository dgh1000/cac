
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString as B
import App.Mp.ParseConfig
import Data.Char
import Translation.TranslationData

s1 =  unlines [ "[pattern [dynamics 2 0 0]" 
              , "         [tempo    2 1 1]"
              , "         [tempo    3 1 1 1]"
              , "]"
              ]


showPatternDescr = show

main = case parse pattern "" (B.pack $ map (fromIntegral . ord) s1) of
  Left err -> print err
  Right (CSPattern p)  -> putStrLn $ showPatternDescr p
