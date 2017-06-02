module Trainer1Parse 
       (Command(..)
        , runParse)
        where
import Data.Maybe
import Data.Char
import Text.Parsec
import Text.Parsec.String

data Command = Forward Int
             | Backward Int
             | Jump Int
             | Play Int [Int]
             | Quit
               deriving (Show)

data Arg = Verts Int
         | Chans [Int]
           deriving (Show)

integer :: Parser Int
integer = do ds <- many1 digit
             return (read ds)

{-
digitList :: Parser [Int]
digitList = do d <- digit
               remainder <- digitList
               return $ read [d] : remainder
            <|>
            return []
-}

-- dec 15 2003

digitList :: Parser [Int]
digitList = many1 (digitToInt `fmap` digit)

-- Parse an argument for the p command
parseArg :: Parser Arg
parseArg = do fmap Verts integer
           <|> 
           do char 'c'
              fmap Chans digitList

parseArgs :: Parser [Arg]
parseArgs = sepBy parseArg space

parseP :: Parser Command
parseP = (do many space
             -- Zero or more arguments may be present.
             -- We will consider at most one "v" argument
             -- and at most one "c" argument. There are
             -- default values for the "V" and "C" arguments
             -- when none are present.
             args <- parseArgs
             let vArgs = [i | Verts i <- args]
                 cArgs = [c | Chans c <- args]
                 singleV = fromMaybe 1 (listToMaybe vArgs)
                 singleC = fromMaybe [] (listToMaybe cArgs)
             return $ Play singleV singleC) 
         <|>
           (return $ Play 1 [])


parseCommand :: Parser Command
parseCommand   = do char 'j'
                    fmap Jump integer
                 <|> 
                 do char 'f'
                    fmap Forward (option 1 integer)
                 <|>   
                 do char 'b'
                    fmap Backward (option 1 integer)
                 <|>
                 do char 'p'
                    parseP
                 <|>   
                 do char 'q'   
                    return Quit
                 

runParse :: String -> Either String Command
runParse s = case parse parseCommand "" s of
  Left err -> Left $ show err
  Right x  -> Right x

run :: Show a => Parser a -> String -> IO ()
run p input =
  case (parse p "" input) of
    Left err -> do putStr "Parse error at "
                   print err
    Right x   -> print x

