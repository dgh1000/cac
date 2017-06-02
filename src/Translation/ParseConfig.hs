module Translation.ParseConfig where


import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.ByteString
import Text.Printf
import Data.Maybe
import Data.Map(Map)
import Data.Ratio
import Data.Set(Set)
import Data.Monoid
import Translation.TranslationData
import Translation.ValidateConfig
import Common.CommonData
import Util.Exception
import Util.Showable






--  EXAMPLE CONFIG IS IN testConfig.txt IN THIS DIRECTORY (Translation)

----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------





{-

-- SAMPLE CONFIG --


  ---->>   as of 2017, example config is in 'testConfig.txt'  <<-----

some sketchy documentation here, not complete:

  Bracketed data
  ----------------------------------------------------------------------

  [- comment ]

  [<keyword> <list of elems>]

  elem can be

    <keyword>:<value>   where <value> is quoted string, int, or double
                                         or list of such types

    <another bracketed data>


  Meta-instrument statement
  ----------------------------------------------------------------------

    [meta instr:<m.i. type>
          name:<quoted m.i. name>
          staves:{list of quoted staff names}
          <m.i. specific config>
    ]

  Meta-instrument config
  ----------------------------------------------------------------------

    bracketed data, bdata 



  timing variation statement
  ----------------------------------------------------------------------

    [timing-variation minLen:<int>    maxLen:<int> 
                      ratio1:<double> ratio2:<double>
                      delta1:<double> delta2:<double> ]

    See /haskell/Translation/TimeMap.hs for algorithm





-}


----------------------------------------------------------------------
----------------------------------------------------------------------


{-            
parseIdentifier :: Parser String
parseIdentifier = many1 (alphaNum <|> char '_')


rationalNum :: Parser Rational
rationalNum  = do
  numerator <- many1 digit
  char '%'
  denominator <- many1 digit
  return $ (read numerator) % (read denominator)
-}




----------------------------------------------------------------------
----------------------------------------------------------------------


skip = (space >> return ())
       <|>
       (lookAhead (try $ string "[-") >> skipInside)


skipInside = (between (char '"') (char '"') (many $ noneOf "\"") >> return ())
             <|>
             (between (char '[') (char ']') (many skipInside) >> return ())
             <|>
             (noneOf "\"[]" >> return ())
                                    
----------------------------------------------------------------------
----------------------------------------------------------------------


identifier = quoted <|> identifier1

identifier1 = do
  c <- letter
  cs <- many $ alphaNum <|> char '_' <|> char '-'
  return $ c:cs


quoted :: Parser String
quoted = do
  char '"'
  s <- manyTill anyChar (char '"')
  lookAhead (space <|> char ']')
  return s
  

intNum :: Parser Int
intNum = try $ do
  s <- many1 digit
  lookAhead (space <|> char ']')
  return $ read s


floatNum :: Parser Double
floatNum = try $ do
  c <- option "" (string "-")
  n1 <- many1 digit
  char '.'
  n2 <- many1 digit
  lookAhead (space <|> char ']')
  return . read $ c ++ n1 ++ "." ++ n2


maybeDouble :: Parser (Maybe Double)
maybeDouble = try $ justDouble <|> nothing


justDouble = (try $ string "J-") >> (Just <$> floatNum)


nothing = try $ string "Nothing" >> return Nothing


configValue :: Parser ConfigValue
configValue = ConfigValueMaybeDouble <$> maybeDouble
              <|>
              ConfigValueInt <$> intNum
              <|>
              ConfigValueDouble <$> floatNum
              <|>
              ConfigValueString <$> identifier
              <|>
              ConfigValueString <$> quoted



-- end must be followed by space or ] or eof
single :: Parser Elem
single = try $ do
  line <- sourceLine <$> getPosition
  col  <- sourceColumn <$> getPosition
  c <- configValue
  lookAhead $ space <|> char ']' <|> (eof >> return ' ')
  many skip
  return $ Single line col c


param :: Parser Elem
param = do
  line <- sourceLine <$> getPosition
  col <- sourceColumn <$> getPosition
  i <- try $ do i <- identifier
                char ':'
                return i
  c <- configValue
  lookAhead $ space <|> char ']' <|> (eof >> return ' ')
  many skip
  return $ Param line col i c  


bracketed :: Parser Elem
bracketed = do
  line <- sourceLine <$> getPosition
  col  <- sourceColumn <$> getPosition
  char '['
  s <- identifier
  many1 skip
  es <- many1 element
  char ']'
  lookAhead $ space <|> char ']' <|> (eof >> return ' ')
  many skip
  return $ Bracketed line col s es


element = param <|> single <|> bracketed


----------------------------------------------------------------------
----------------------------------------------------------------------


data ConfigStatement = CSMetaInstr Elem
                     | CSTimingVar TimingVariation


metaInstr :: Parser ConfigStatement
metaInstr = do
  x@(Bracketed _ _ t _) <- try bracketed
  let msg = "bracketed element of type 'meta'"
  case t of
    "meta"    -> return $ CSMetaInstr x
    otherwise -> fail msg


toTimingVar :: Elem -> TimingVariation
toTimingVar x = TimingVariation minL maxL ratio1 ratio2 delta1 delta2
  where
    msg    = "while parsing timing-variation, "
    minL   = runExcMsg msg $ findParam1 "minLen" x
    maxL   = runExcMsg msg $ findParam1 "maxLen" x
    ratio1 = runExcMsg msg $ findParam1 "ratio1" x
    ratio2 = runExcMsg msg $ findParam1 "ratio2" x
    delta1 = runExcMsg msg $ findParam1 "delta1" x 
    delta2 = runExcMsg msg $ findParam1 "delta2" x


configSt :: Parser ConfigStatement
configSt = do
  let msg = "at top level of config file, unknown bracketed element " ++
            "of type '%s'"
  b@(Bracketed _ _ t _) <- bracketed
  case t of
    "timing-variation" -> return . CSTimingVar $ toTimingVar b
    "meta" -> return $ CSMetaInstr b
    s -> fail $ printf msg s
      
  


parseConfig :: Parser ConfigFile
parseConfig = do
  many space
  ss <- many configSt
  eof
  -- filter out meta statements and timing-varation statements
  let ms = [e | CSMetaInstr e <- ss]
      ts = [t | CSTimingVar t <- ss]
      mTv = case ts of
        []  -> Nothing
        [x] -> Just x
        _   -> fail "more than one timing-variation element in config"
  return $ ConfigFile ms mTv


