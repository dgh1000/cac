{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Map(Map)
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Data.Set(Set)
import Text.Parsec
import Text.Parsec.String
import Text.Printf
import Util.Parse
import Util.Map
import Score.ScoreData
import Common.CommonExport
import Util.Exception


ss = [ "T=1.5:1*"
     , "T=10Q"
     , "T=1:1a"
     , "x"
     , "3:2>>"
     , ">>"
     , "3:2>>x"
     , "<<>>"
     , "<<3.2:1>>"
     , "<<3:2a"
     ]

ss2 = [ "<|1;%2"
      ]

-- ss3: test building up warp
ss3 = [ "123>"
      , "<123"
      , "<>"
      , "<" ]


doParse :: Parser Mark -> String -> String
doParse p s = case parse p "" s of
  Left e  -> show e ++ "\n"
  Right x -> show x ++ "\n"

main = putStrLn . unlines . map (doParse mark2) $ ss3

timeSigs = M.fromList $ zip [1..]
  [ TimeSig 4 4
  , TimeSig 4 4
  , TimeSig 4 4]

----------------------------------------------------------------------


mark = setTempo <|> rampBeg <|> rampEndBeg <|> rampEnd <|> 
  warp timeSigs (Loc 2 1) (S.fromList [Loc 1 1,Loc 3 1])

mark2 = warp2 timeSigs (Loc 2 1) (S.fromList [Loc 1 1,Loc 3 1])


setTempo :: Parser Mark
setTempo = do
  try $ (string "T=" <?> "set tempo mark (T=)")
  t <- tempo
  eof <?> "end of tempo specification in T= mark"
  return $ SetTempo t


-- For absolute tempos, can only handle quarters per minute right now 
--
-- 100 Q
-- a
-- 3:2a
-- 3:2
tempo :: Parser Tempo
tempo =
  (do s <- varName
      return $ TempoRelative (NumRatio 1 1) (Just [s]))
  <|>
  (do f1 <- simpleDouble
      ((do char 'Q' <?> "'Q' for absolute tempo"
           return $ TempoAbs f1)
       <|>
       (do char ':' <?> "':' for tempo ratio"
           f2 <- simpleDouble
           ((do char '*'
                return $ TempoRelative (NumRatio f1 f2) 
                       Nothing)
            <|>
            (do v <- varName
                return $ TempoRelative (NumRatio f1 f2) 
                      (Just [v]))))))


rampBeg :: Parser Mark
rampBeg = do
  try $ lookAhead $ (many $ oneOf "0123456789.:") >> 
          (string ">>" <?> "ramp begin with >>")
  ((do string ">>"
       eof <?> "end of ramp begin"
       return $ RampBeg (NumRatio 1 1))
   <|>
   (do d1 <- simpleDouble
       char ':' <?> "':' in ratio specification in ramp begin"
       d2 <- simpleDouble
       string ">>"
       eof <?> "end of ramp begin"
       return $ RampBeg (NumRatio d1 d2)))


rampEndBeg :: Parser Mark
rampEndBeg = do
  try $ lookAhead 
    (string "<<" >> many (oneOf "0123456789.:") >> 
       (string ">>" <?> "ramp end/begin with << and >>"))
  string "<<"
  r <- option (NumRatio 1 1) 
               (do d1 <- simpleDouble
                   char ':' <?> "':' in ratio specification in ramp end/begin"
                   d2 <- simpleDouble
                   return $ NumRatio d1 d2)
  string ">>"
  eof <?> "end of ramp end/begin"
  return $ RampEndBeg r


rampEnd :: Parser Mark
rampEnd = do
  try $ lookAhead $ ((do string "<<"
                         many (noneOf ">")) <?> "ramp end with <<")
  string "<<"
  e <- (do t <- tempo
           return $ Right t)
       <|>
       (do d1 <- simpleDouble
           char ':' <?> "':' for prevailing tempo ratio"
           d2 <- simpleDouble
           return $ Left $ NumRatio d1 d2)
  f <- isJust `liftM` optionMaybe (char '|')
  eof <?> "end of ramp end"
  return $ RampEnd e f

warp2 :: Map Int TimeSig -> Loc -> Set Loc -> Parser Mark
warp2 timeSigs loc wLocs =
  (do try (lookAhead (do char '<' 
                         many1 (noneOf "<>") 
                         eof) <?> "warp")
      char '<'
      ds <- many1 digit
      eof <?> "end of left-pointing warp"
      return $ Warp False Nothing Nothing (-1) ((read ds)%1))
  <|>
  (do try (lookAhead (do many1 (noneOf "<>")
                         char '>'
                         eof) <?> "warp")
      ds <- many1 digit
      char '>'
      eof <?> "end of right-pointing warp"
      return $ Warp False Nothing Nothing 1 ((read ds)%1))

warp :: Map Int TimeSig -> Loc -> Set Loc -> Parser Mark
warp timeSigs loc wLocs = 
  (do try (lookAhead (char '<' >> many1 (noneOf "<>") >> eof) <?> "warp")
      char '<'
      w <- warpInside timeSigs loc wLocs (-1)
      eof <?> "end of left-pointing warp"
      return w)
  <|>
  (do try (lookAhead (many1 (noneOf "<>") >> char '>' >> eof) <?> "warp")
      w <- warpInside timeSigs loc wLocs (1)
      char '>'
      eof <?> "end of right-pointing warp"
      return w)

warpInside :: Map Int TimeSig -> Loc -> Set Loc -> Double -> Parser Mark
warpInside timeSigs loc wLocs direction = 
  (do char '|'
      (do try (lookAhead followedByBar)
          w <- warpVeryInside timeSigs loc wLocs direction True True
          char '|' <?> "1"
          return w)
      <|>
      (warpVeryInside timeSigs loc wLocs direction True False))
  <|>
  ((do try (lookAhead followedByBar)
       w <- warpVeryInside timeSigs loc wLocs direction False True
       char '|' <?> "2"
       return w)
   <|>
   (warpVeryInside timeSigs loc wLocs direction False False))


followedByBar = try (lookAhead $ (many1 (noneOf "|") >> (char '|' <?> "3")))
     

warpVeryInside timeSigs loc wLocs direction leftBar rightBar =
  (do try $ lookAhead ((many1 $ oneOf "0123456789%") >> char ';' >> 
                       (many1 $ oneOf "0123456789%"))
      v1 <- try rational <|> (do {i <- parseInt; return $ fromIntegral i % 1})
      char ';'
      v2 <- rational
      let globFlag = leftBar || rightBar
          leftLoc =  if leftBar
                       then Just $ warpLeftLocWidth  timeSigs loc v1
                       else Nothing
          rightLoc = if rightBar
                       then Just $ warpRightLocWidth timeSigs loc v1
                       else Nothing
      return $ Warp globFlag  leftLoc rightLoc direction v2)
  <|>
  (do amount <- rational
      let globFlag = leftBar || rightBar
          leftLoc =  if leftBar
                       then Just $ warpLeftLocW  loc wLocs
                       else Nothing
          rightLoc = if rightBar
                       then Just $ warpRightLocW loc wLocs
                       else Nothing
      return $ Warp globFlag  leftLoc rightLoc direction amount)
      


warpLeftLocW loc wLocs = case S.lookupLT loc wLocs of
  Nothing -> throwMine $ printf "in warp at %s, no left w" (simpleShowLoc loc)
  Just l  -> l


warpRightLocW loc wLocs = case S.lookupGT loc wLocs of
  Nothing -> throwMine $ printf "in warp at %s, no right w" (simpleShowLoc loc)
  Just l  -> l


warpRightLocWidth timeSigs loc width = case locAdd timeSigs loc width of
  Nothing -> throwMine $ printf ("in warp at %s, adding width of %s puts " ++
             "it past end of score") (simpleShowLoc loc) (show width)
  Just l  -> l


warpLeftLocWidth timeSigs loc width = case locSub timeSigs loc width of
  Nothing -> throwMine $ printf ("in warp at %s, subtracting width of %s " ++
             "puts it past beginning of score") (simpleShowLoc loc) 
             (show width)
  Just l  -> l



rational :: Parser Rational
rational = do 
  n1 <- optionMaybe parseInt
  char '%'
  n2 <- optionMaybe parseInt
  when (isNothing n1 && isNothing n2)
       (fail "isolated %")  
  let i1 = case n1 of {Nothing -> 1; Just x -> x}
      i2 = case n2 of {Nothing -> 1; Just x -> x}
  return $ (fromIntegral i1 % fromIntegral i2)



{-
parseInt2 :: Parser Int
parseInt2 = do
  s <- many1 digit <?> "integer"
  return $ read s
-}

{-
absoluteTempo :: Parser Tempo
absoluteTempo = do
  i <- parseInt
  many space
  char 'Q'
  return $ TempoAbs i


relativeTempo :: Parser Tempo
relativeTempo = do
  r <- option (NumRatio 1 1) numRatio
  v <- (do s <- varName
           return $ Just [s])
       <|>
       (char '*' >> return Nothing)
  return $ TempoRelative r v
-}

{-
numRatio :: Parser NumRatio
numRatio = do
  n1 <- simpleDouble
  char ':' <?> "colon in ratio"
  n2 <- simpleDouble
  return $ NumRatio n1 n2
-}


simpleDouble :: Parser Double
simpleDouble = do
  s <- many1 (oneOf ".0123456789")
  case reads s of
    (f,[]):_ -> return f
    _        -> fail "malformed numeric value"


varName :: Parser Char
varName = oneOf ['a'..'z'] <?> "variable name"




  