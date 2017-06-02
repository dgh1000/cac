
import Data.Ratio
import Text.Parsec
import Text.Parsec.String
import Util.Parse


rational :: Parser Rational
rational =
  (try $ do i1 <- parseInteger
            notFollowedBy $ char '%'
            return $ i1%1)
  <|>
  (try $ do i1 <- parseInteger
            char '%'
            notFollowedBy digit
            return $ i1%1)
  <|>
  (try $ do i1 <- parseInteger
            char '%'
            i2 <- parseInteger
            return $ i1%i2)
  <|>
  (do char '%'
      i <- parseInteger
      return $ 1%i)


simpleDouble :: Parser Double
simpleDouble = do
  s <- many1 (oneOf ".0123456789")
  case reads s of
    (f,[]):_ -> return f
    _        -> fail "malformed numeric value"


num :: Parser Double
num =
  -- integer not followed by . or :
  (do i <- try $ do i <- parseInt
                    notFollowedBy $ char '.' <|> divChar
                    return i
      if i > 9
        then return $ fromIntegral i/100
        else return $ fromIntegral i)
  <|>
  -- :float
  (do divChar
      d <- simpleDouble
      return $ 1/d)
  <|>
  -- float not followed by : (the case of int not followed by : will have been
  -- caught above
  (try $ do d <- simpleDouble
            notFollowedBy $ divChar
            return d)
  <|>
  -- float:float
  (try $ do d1 <- simpleDouble
            divChar
            d2 <- simpleDouble
            return $ d1/d2)


divChar = char '%' <|> char ':'


nums = [ "3"
       , "30"
       , "130"
       , "3:"
       , ":0.3333"
       , "3:4"
       , "3000.0"
       , "%2"
       , "30x"
       ]


main = do
  let x s = do
        print s
        print $ parse num "" s
  mapM_ x nums

