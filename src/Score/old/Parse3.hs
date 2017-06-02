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
     , "T=1:1aa"
     , "T=X"
     , "t=3:2a"
     , ">>"
     , "3:2>>"
     ]

ss2 = [ "<%2;3%"
      , "%2|>"
      , "3;2%3>"
      , "%"
      , "|1;%4|>"
      ]

ss3 = [ "<<3" 
      , "<<3:2"
      , "<<3:2|>>"
      , "<<3:2|"
      , "<<100Q"
      , "<<3:2a|"
      , "<<3:2>>"
      , "<<3:2a>>"
      , "<<3>>"
      ]

ss4 = [ "ps5"
      , "ps@5"
      , "ps%8" 
      , "arp5"
      , "arp@5"
      , "rit."
      , "accel."
      ]

doParse :: Parser Mark -> String -> String
doParse p s = case parse p "" s of
  Left e  -> show e ++ "\n"
  Right x -> show x ++ "\n"

main = putStrLn . unlines . map (doParse mark) $ ss4

timeSigs = M.fromList $ zip [1..]
  [ TimeSig 4 4
  , TimeSig 4 4
  , TimeSig 4 4]

----------------------------------------------------------------------


mark :: Parser Mark
mark =
  -- set tempo
  (do string "T="
      t <- tempo 
      eof <?> "end of set tempo"
      return $ SetTempo t)
  <|>
  -- set variable
  (do v <- try $ do s <- varName
                    char '='
                    return s
      t <- tempo
      eof <?> "end of set variable"
      return $ SetVar [v] t)
  <|>
  -- splice mark
  (do char '$'
      c <- oneOf ['a'..'z']
      eof <?> "end of splice mark"
      return $ SpliceMark c)
  <|>
  -- ramp begin with just ">>"
  (do try (string ">>")
      return $ RampBeg (NumRatio 1 1))
  <|>
  -- ramp end or end/begin
  (do string "<<"
      n <- Right <$> try tempo
           <|>
           Left <$> numRatio
      endBeg <- option False (do {string ">>"; eof; return True})
      if endBeg
        then case n of
               Right _ -> fail $ "ramp end/beg cannot have full tempo " ++
                                 "specification; must have simple ratio"
               Left  r -> return $ RampEndBeg r
        else do bar <- isJust <$> optionMaybe (char '|')
                eof
                return $ RampEnd n bar)
  <|>
  -- left-pointing warp
  (do char '<'
      warpInside True)
  <|>
  -- right-pointing warp with left bar
  (do char '|'
      v1 <- try rational 
            <|> (do {ds <- many1 digit; return $ fromIntegral $ read ds})
            <?> "integer beats or rational beats"
      v2 <- optionMaybe (do char ';'
                            rational)
      let (width,amt) = case (v1,v2) of
            (x, Nothing) -> (Nothing, x)
            (x, Just y ) -> (Just x,  y)
      rightBar <- isJust <$> optionMaybe (char '|')
      char '>'
      return $ WarpTemp True rightBar width amt 1)
  <|>
  -- something without prefix that makes it ambiguous... this could be
  -- ramp begin or right-pointing warp without left bar
  (do v <- (try numRatio >>= return . Left) 
           <|> 
           (try rational >>= return . Right)
           <|>
           (intRational  >>= return . Right)
           <?> "ratio for ramp beg, or rational for warp"
      case v of
        Left r   -> do string ">>" <?> "ramp begin >> string"
                       return $ RampBeg r
        Right v1 -> do v2 <- optionMaybe $ do char ';'
                                              rational
                       let (width,amt) = case (v1,v2) of
                             (x, Nothing) -> (Nothing, x)
                             (x, Just y ) -> (Just x,  y)
                       rightBar <- isJust <$> optionMaybe (char '|')
                       char '>'
                       return $ WarpTemp False rightBar width amt 1)
  <|>
  (do try $ string "ps"
      ((do s <- seconds
           eof
           return $ Pause $ DurSecs s
       <|>
       (do char '@'
           s <- seconds
           eof
           return $ Pause $ DurSecs s
       <|>
       (do r <- rational
           eof
           return $ Pause $ DurBeats r)))))
  <|>
  (do try $ string "arp"
      s <- seconds
      eof
      return $ ArpDelta s)
  <|>
  (do try $ string "stac"
      s <- seconds
      eof
      return $ StacDur s)
  <|>
  (try (string "rit.") >> return RitAccel)
  <|>
  (try (string "accel.") >> return RitAccel)
      
 
 
seconds =
  (do ds <- many1 digit
      return $ 0.01 * fromIntegral (read ds))
  <|>
  (do char '@'
      ds <- many1 digit
      return $ 0.01 * fromIntegral (read ds))
                 

warpInside :: Bool -> Parser Mark
warpInside leftPointing = do
  leftBar <- isJust <$> optionMaybe (char '|')
  v1 <- try rational 
        <|> (do {ds <- many1 digit; return $ fromIntegral $ read ds})
        <?> "integer beats or rational beats"
  v2 <- optionMaybe (do char ';'
                        rational)
  let (width,amt) = case (v1,v2) of
        (x, Nothing) -> (Nothing, x)
        (x, Just y ) -> (Just x,  y)
  rightBar <- isJust <$> optionMaybe (char '|')
  if leftPointing then eof
                  else char '>' >> eof
  return $ WarpTemp leftBar rightBar width amt 
                    (if leftPointing then (-1) else 1)


intRational :: Parser Rational
intRational = do
  ds <- many1 digit
  return . fromIntegral $ read ds


{-
  (do r <- try numRatio
      string ">>" <?> "ramp begin"
      return $ RampBeg r)
-}


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
  <?> "valid tempo specification"


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





simpleDouble :: Parser Double
simpleDouble = do
  s <- many1 (oneOf ".0123456789")
  case reads s of
    (f,[]):_ -> return f
    _        -> fail "malformed numeric value"


varName :: Parser Char
varName = oneOf ['a'..'z'] -- <?> "variable name"


numRatio :: Parser NumRatio
numRatio = do
  n1 <- simpleDouble
  char ':'
  n2 <- simpleDouble
  return $ NumRatio n1 n2





  