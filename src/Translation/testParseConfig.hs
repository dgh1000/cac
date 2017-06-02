
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Map(Map)
import Data.Char
import Control.Monad.Except
import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.ByteString
import Translation.ParseConfig
import Translation.ShowTranslation
import Translation.TranslationData
import Util.Showable
import Common.CommonData
import Util.Exception
import Translation.ValidateConfig


buf = unlines [ "[bob foo:3  juice:\"oops\"  "
              , "l:{3 4.0 \"bar\"}"
              , "]"
              ]


bufBs :: ByteString
bufBs = B.pack . map (fromIntegral . ord) $ buf 


testParse :: Parser Elem
testParse = do
  many space
  e <- bracketed
  eof
  return e


convDests :: [String] -> Elem -> Map String [Int]
convDests staffNames e = m2
  where
    msg = "while verifying staff names present in 'dests' element, "
    e2 = runExc $ findBr "dests" e
    m = M.fromList $ runExc $ mapM (convertBracketedSizeN 2) e2
    m2 = runExc $ withExcept (msg ++) $ mapVfyKeys staffNames m


f :: Elem -> Double
f e = runExc $ findParam1 "foo" e

g :: Elem -> [Double]
g e = runExc $ findBrOfSingles_sizeN 2 "nodinList" e

main = do
  buf <- B.readFile "testConfig.txt"
  case parse parseConfig "" buf of
    Left err -> print err
    Right v -> putStrLn $ showIString v


main2 :: Exc (Maybe Double)
main2 = maybeParam "foo" (Param 0 0 "foo" (ConfigValueInt 1))


{-

skipOne :: Parser ()
skipOne = (do space
              return ())
          <|>
          (do between (try $ string "[-") (many skipInside) (char ']'))


skipInside :: Parser ()
skipInside = 
             <|> 


skip :: Parser ()
skip = (do space
           return ())
       <|>
       between (char '"') (many anyChar) (char '"')
       <|>
       (do between (char '[') (many oneSkip) (char ']')
           return ())

-}

{-
skip = (space >> return ())
       <|>
       (lookAhead (string "[-") >> skipInside)

skipInside = (between (char '"') (char '"') (many $ noneOf "\"") >> return ())
             <|>
             (between (char '[') (char ']') (many skipInside) >> return ())
             <|>
             (noneOf "\"[]" >> return ())
-}                                    




-- next char is either '[' which opens a new skipping, or one skip

{-

skipping =
  (do between (char '[') skipping (char ']')
      return ())
  <|>
  (do many1 oneSkip
      return ())
-}

ff = many skip >> anyChar


main3 = do
  buf <- B.readFile "testConfig2.txt"
  case parse ff "" buf of
    Left err -> print err
    Right c  -> print c
    
