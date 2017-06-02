import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString as B
import Data.Functor

{-
-- parseVidToken
--   Parse a token, which can be one of three types:
--     Tag: starts with a $. Remaining characters can be anything except
--           '[' or ']' or whitespace
--     Label: starts with a / or //. Remaining characters can be anything
--            except ']' or '[' or whitespace
--     Text: anything that's not a tag or label. can contain any characters
--              except ']' or '[' or whitespace
--     
parseVidToken :: Parser VidToken
parseVidToken = do
  skipMany space
  l <- ((do try (string "//")
            s <- parseTokenText
            return (VidLabel s True))
        <|> 
        (do char '/'
            s <- parseTokenText
            return (VidLabel s False))
        <|> 
        (do char '$'
            VidTag <$> parseTokenText)
        <|>
        VidText <$> parseTokenText)
  skipMany space
  return l
-}

-- parseAnyText
--   Parse legal token text, which is any character except '[', ']', and
--   whitespace.
parseAnyText :: Parser String
parseAnyText = do
  many1 (noneOf "[] \n\t\r")

{-
-- parseAnyTextAlphaNum 
--   Parses a VidToken but enforces that it is a VidText that only contains
--    alphanumeric characters.
parseAnyTextAlphaNum :: Parser String
parseAnyTextAlphaNum = do
  skipMany space
  s <- manyTill alphaNum (char '[' <|> char ']' <|> space)
  skipMany space
  return s

parseVidEntry :: Parser VidEntry
parseVidEntry = do
  skipMany space
  char '['
  ((do try (string "set")
       VidEntry <$> parseSet))
   <|>
   (do many1 parseElem

       t <- parseAnyTextAlphaNum
       r <- many parseVidToken

parseVidElem :: Parser VidElem
parseVidElem = VidElem <$> many1 parseVidElemEntry

           
parseVidElemEntry :: Parser VidElemEntry
parseVidElemEntry = do
  skipMany space
  (try (char '['
        VEEElem <$> parseVidElem)
   <|>
   VEEToken 
-}

parseSingleElem :: Parser DataElem
parseSingleElem = do
  p <- getPosition
  r <- ((do try (string "//")
            t <- parseAnyText
            return $ DELabel t True (sourceLine p))
        <|>
        (do char '/'
            t <- parseAnyText
            return $ DELabel t False (sourceLine p))
        <|>
        (do char '$'
            t <- parseAnyText
            return $ DETag t (sourceLine p))
        <|>
        (do t <- parseAnyText
            return $ DEText t (sourceLine p))
        <?> "single element (tag, label, or text)")
  skipMany space
  return r
  
parseBracketedElem :: Parser DataElem
parseBracketedElem = do
  (do p <- getPosition
      char '['
      skipMany space
      es <- many1 parseAnyElem
      skipMany space
      char ']'
      skipMany space
      return $ DEList es (sourceLine p)) 
  <?> "bracketed list"

parseAnyElem :: Parser DataElem
parseAnyElem = do
  parseBracketedElem <|> parseSingleElem      
    

parseElems :: Parser [DataElem]
parseElems = do
  many parseBracketedElem

{-
testManyTill :: Parser String
testManyTill =
  manyTill alphaNum (char '[' <|> char ']' <|> space)
-}

-- In DataElem, the 'Int's are line numbers
data DataElem = DETag String Int
              | DELabel String Bool Int
              | DEText String Int
              | DEList [DataElem] Int
              deriving(Show)

{-
data VidEntry = VEInstr VidInstr
              | VEElem VidElem

data VidElemEntry = VEEToken VidToken
                  | VEEElem VidElem

data VidElem = VidElem [VidElemEntry]

data VidInstr = VISet String [VidToken]

data VidToken = VidTag String
              | VidLabel String Bool -- Bool is true for a double-slash label
                                     -- also known as narrow label
              | VidText String
                deriving(Show)
-}

{-
 test :: IO ()
test = do
  b <- B.readFile "test.txt"
  case parse parseVidToken "" b of
    Left err -> putStrLn (show err)
    Right s -> putStrLn (show s)

test2 :: IO ()
test2 = do
  b <- B.readFile "test.txt"
  case parse parseAnyTextAlphaNum "" b of
    Left err -> putStrLn (show err)
    Right s -> putStrLn (show s)

test3 :: IO ()
test3 = do
  b <- B.readFile "test.txt"
  case parse testManyTill "" b of
    Left err -> putStrLn (show err)
    Right s -> putStrLn (show s)
  
-}

test4 :: IO ()
test4 = do
  b <- B.readFile "test.txt"
  case parse parseElems "" b of
    Left err -> putStrLn (show err)
    Right s -> putStrLn (show s)
