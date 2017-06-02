module Util.ParseBracketed where

import Data.Binary
import Data.Maybe
import qualified Data.List as L
import Text.Printf
import Text.Parsec
import Text.Parsec.ByteString
import qualified Text.Parsec.String as PS
import Data.Map(Map)
import qualified Data.Map as M
import Util.Exception

{-
data SomeDataType = SomeDataTypeString
                  | SomeDataTypeFloat
                  | SomeDataTypeInt
                    deriving(Show)
data SomeData = SomeDataString String
              | SomeDataFloat  Float
              | SomeDataInt    Int
                deriving(Show)
-}

data SquareBracketed = SquareBracketed String [(String,String)]
    deriving(Show,Eq)


instance Binary SquareBracketed where
  put (SquareBracketed a1 a2) = 
    do {put a1; put a2 }
  get = do
    a1 <- get
    a2 <- get
    return (SquareBracketed a1 a2)

{-
type DataSpec = (String,SomeDataType)


-- A map of the square bracket group name to a list of DataSpec's describing
-- the date that must go there.
type DataSpecSheet = [(String,[DataSpec])]

data SquareBracketedData = SquareBracketedData String [(String,SomeData)]
                           deriving(Show)
-}

areDuplicates :: SquareBracketed -> Bool
areDuplicates (SquareBracketed _ data_) = length keys > length (L.nub keys)
  where keys = map fst data_

myLookup :: String -> String -> [(String,String)] -> String
myLookup name groupName data_ =
  case lookup name data_ of
    Nothing -> throwMine $ printf ("In group '%s' there is" ++
                                   " no field named '%s'") groupName name
    Just x -> x


lookupInt :: String -> SquareBracketed -> Int
lookupInt name (SquareBracketed groupName dat) = fromDataInt groupName (name,s)
  where
    s = myLookup name groupName dat

fromDataInt :: String -> (String,String) -> Int
fromDataInt gn (n,s) =
  case parse parseIntField "" s of
    Left err -> 
      throwMine $ printf ("In group '%s', the field '%s' is supposed to " ++
                  "be an int (followed directly by ')'.") gn n
    Right i -> i

lookupFloat :: String -> SquareBracketed -> Float
lookupFloat name (SquareBracketed groupName dat) = 
  fromDataFloat groupName (name,s)
  where
    s = myLookup name groupName dat

fromDataFloat :: String -> (String,String) -> Float
fromDataFloat gn (n,s) =
  case parse parseFloatField "" s of
    Left err -> 
      throwMine $ printf ("In group '%s', the field '%s' is supposed to " ++
                  "be a float (followed directly by ')'.") gn n
    Right i -> i

lookupMany1Floats :: String -> SquareBracketed -> [Float]
lookupMany1Floats name (SquareBracketed groupName dat) = 
  fromDataMany1Floats groupName (name,s)
  where
    s = myLookup name groupName dat

fromDataMany1Floats :: String -> (String,String) -> [Float]
fromDataMany1Floats gn (n,s) =
  case parse parseMany1FloatsField "" s of
    Left err -> 
      throwMine $ printf ("In group '%s', the field '%s' is supposed to " ++
                  "be a (followed directly by ')' float.") gn n
    Right i -> i


lookupString :: String -> SquareBracketed -> String
lookupString name (SquareBracketed groupName dat) = 
  fromDataString groupName (name,s)
  where
    s = myLookup name groupName dat

fromDataString :: String -> (String,String) -> String
fromDataString gn (n,s) =
  case parse parseStringField "" s of
    Left err -> 
      throwMine $ printf ("In group '%s', the field '%s' is supposed to " ++
                  "be a double-quoted string") gn n
    Right i -> i

parseParenthesized :: Parser (Maybe (String,String))
parseParenthesized = do
  char '('
  s <- (do try (string "-")
           manyTill anyChar (char ')')
           return Nothing)
       <|>
       (do skipMany space
           fieldName <- many1 (alphaNum <|> oneOf "-_")
           skipMany space
           fieldValue <- manyTill anyChar $ char ')'
           return $ Just (fieldName,fieldValue))
  skipMany space
  return s

parseStringField :: PS.Parser String
parseStringField = do
  char '"'
  s <- many (noneOf "\"") 
  char '"'
  eof
  return s

parseFloatField :: PS.Parser Float
parseFloatField = do
  s <- many1 (oneOf "-0123456789.")
  eof
  return $ read s

parseIntField :: PS.Parser Int
parseIntField = do
  s <- many1 digit
  eof
  return $ read s

parseMany1FloatsField :: PS.Parser [Float]
parseMany1FloatsField = do
  ss <- many1 (do
    skipMany space
    s <- many1 (oneOf "-0123456789.")
    return $ read s)
  eof
  return ss

{-
toRealData :: [DataSpec] -> (String,String) -> (String,SomeData)
toRealData dsList (fieldName, fieldValue) =
  case lookup fieldName dsList of
    Nothing -> throwMine $ printf ("Can't parse the field '%s'; no " ++
                                   "data spec.") fieldName
    Just dt -> 
      case dt of
        SomeDataTypeString ->
          case parse parseStringField "" fieldValue of
            Left err -> 
              throwMine $ printf "Can't parse **string** value of field %s:%s"
                        fieldName fieldValue
            Right value -> (fieldName, SomeDataString value)
        SomeDataTypeFloat ->
          case parse parseFloatField "" fieldValue of
            Left err ->
              throwMine $ printf "Can't parse **float** value of field %s:%s"
                        fieldName fieldValue
            Right value -> (fieldName, SomeDataFloat value)

        SomeDataTypeInt ->
          case parse parseIntField "" fieldValue of
            Left err ->
              throwMine $ printf "Can't parse **int** value of field %s:%s"
                        fieldName fieldValue
            Right value -> (fieldName, SomeDataInt value)
-}    

parseSquareBracketed :: Parser (Maybe SquareBracketed)
parseSquareBracketed = do
  skipMany space
  char '['
  s <- (do try (string "-")
           manyTill anyChar (char ']')
           return Nothing)
       <|>
       (do skipMany space
           name <- many1 (alphaNum <|> oneOf "_-")
           skipMany space
           ps <- many parseParenthesized
           char ']'
           let sb = SquareBracketed name (catMaybes ps)
           if areDuplicates sb
             then throwMine $ printf ("Error: there are duplicate " ++
                                        "values in group '%s'") name
             else return $ Just sb)
  skipMany space
  return s

parseManySquareBracketed :: Parser [SquareBracketed]
parseManySquareBracketed = do
  ps <- many1 parseSquareBracketed
  return $ catMaybes ps


{-
parseSquareBracketedData :: DataSpecSheet -> 
                            Parser (Maybe SquareBracketedData)
parseSquareBracketedData dss = do
  result <- parseSquareBracketed
  case result of
    Nothing -> return Nothing
    Just x -> do
      let SquareBracketed brackGroupName rawData = x

          dataSpecList = 
              case lookup brackGroupName dss of
                Nothing -> throwMine$printf ("Can't find DataSpec list for " ++
                           "bracket group '%s'") brackGroupName
                Just x -> x
          realData :: [(String,SomeData)] 
          realData = map (toRealData dataSpecList) rawData
          namesPresent = map fst realData
          l = length namesPresent
          unique = L.nub namesPresent
          areDuplicates = l  > length unique
          isInsufficient = length unique < length dataSpecList
      
      return $ 
        if areDuplicates
        then throwMine $ printf ("In bracket group '%s' there are duplicate" ++
                                 " field names.") brackGroupName
        else 
          if isInsufficient 
          then throwMine $ printf ("In bracket group '%s' there is " ++
                                   "a missing field name.") brackGroupName
          else Just $ SquareBracketedData brackGroupName realData
-}