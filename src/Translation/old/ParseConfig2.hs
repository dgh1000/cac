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
  return $ ConfigFile ms [] mTv


{-

OLD OLD OLD



----------------------------------------------------------------------
----------------------------------------------------------------------

timingVariation :: Parser ParsedConfSt
timingVariation = do
  try $ string "[timing-variation"
  many1 space
  string "minLen:"
  lMin <- intNum
  many space
  string "maxLen:"
  lMax <- intNum
  many space
  string "ratio1:"
  r1 <- floatNum
  many space
  string "ratio2:"
  r2 <- floatNum
  many space
  string "delta1:"
  d1 <- floatNum
  many space
  string "delta2:"
  d2 <- floatNum
  many space
  char ']'
  many space
  if lMax < lMin 
    then fail "minN should be LE maxN"
    else return $ PcsTimingVariation $ TimingVariation lMin lMax r1 r2 d1 d2


----------------------------------------------------------------------
----------------------------------------------------------------------

susPedConfig :: Parser ParsedConfSt
susPedConfig = do
  try $ string "[susped"
  skipMany space
  d1 <- floatNum
  skipMany space
  d2 <- floatNum
  skipMany space
  char ']'
  skipMany space
  return $ PcsSusPed d1 d2


pipValue :: Parser ConfigValue
pipValue = do
  s <- many1 (alphaNum <|> oneOf "_-.")
  let isNumberChar c = c `elem` "0123456789."
      isDigitChar  c = c `elem` "0123456789"
  if all isNumberChar s
    then 
      if all isDigitChar s
        then return $ ConfigValueInt    (read s)
        else return $ ConfigValueDouble (read s)
    else return $ ConfigValueString s
 
 
staffField :: Parser (String,ConfigValue)
staffField = try (do
  fname <- many1 (alphaNum <|> oneOf "_-")
  char ':'
  value <- pipValue
  skipMany space
  return (fname,value))


staffMidi :: Parser (String,(Int,Int))
staffMidi = try (do
  string "[midi"
  skipMany space
  chanName <- parseIdentifier
  skipMany space
  string "stream:"
  stream <- intNum
  skipMany space
  string "chan:"
  num <- intNum
  skipMany space
  char ']'
  skipMany space
  return (chanName,(stream,num)) )
  

computeMuteState :: [StaffOption] -> MuteState
computeMuteState opts = case [s | MuteOption s <- opts] of
  []  -> MuteNone
  s:_ -> s


computeSusPedUse :: [StaffOption] -> SusPedUse
computeSusPedUse opts = case [u | SusPedOption u <- opts] of
  []  -> SusPedIgnore
  u:_ -> u


susPedUse :: Parser StaffOption
susPedUse = do
  try $ string "susped:"
  x <- (do {string "source"; return SusPedSource}
        <|>
        do {string "apply";  return SusPedApply })
  skipMany space
  return $ SusPedOption x  


muteState :: Parser StaffOption
muteState = do
  try $ string "mute:"
  d <- (do try (string "all")
           return MuteAll)
       <|>
       (do try (string "controllers")
           return MuteControllers)
       <|>
       (do try (string "notes")
           return MuteNotes)
  skipMany space
  return $ MuteOption d


commentElement :: Parser ()
commentElement =
  (do char '['
      manyTill commentElement (char ']')
      return ())
  <|>
  (do anyChar  
      return ())


comment = do
  try $ string "[-"
  manyTill commentElement (char ']')
  skipMany space
  return PcsComment



staffIgnore = do
  try $ string "[staff-ignore"
  many space
  name <- quotedString
  many space
  char ']'
  many space
  return $ PcsStaffIgnore name


statement :: Parser ParsedConfSt
statement = staffIgnore <|> staff <|> susPedConfig <|> comment <|> pattern <|>
            timingVariation


-- Output
--  (<commands>, <map of all part names to instruments>, <mixer data>,
--    <number of time divisions of beat in time map> <number blank msrs to
--      treat as automatic stop point>)
parseConfigFile :: Parser PlConfFile
parseConfigFile = do
   skipMany space
   entries <- many statement
   manyTill space eof

   -- Construct map of staff config
   let staffList = [sc | PcsStaff sc <- entries]
       staffMap = M.fromListWith err $ map (stcName &&& id) staffList
       err _ _ = throwMine "in config, two staves with same name"
      
   -- Construct set of StaffIgnore
   let ignoreNames = S.fromList [n | PcsStaffIgnore n <- entries]

   -- Compute pedal source and application parts
   let pedSrcName = case [stcName st | st <- staffList, 
                                       stcSusPed st == SusPedSource] of
         []  -> Nothing
         [x] -> Just x
         _   -> throwMine "in config, multiple parts marked as susped:source"
       pedAppNames = [stcName st | st <- staffList , 
                                   stcSusPed st == SusPedApply]

   -- Compute maybe pedal config
   let pedConfigValues = case [(x,y) | PcsSusPed x y <- entries] of
         []      -> Nothing
         [(x,y)] -> Just (x,y)
         _       -> throwMine "in config, multiple susped statements"

   -- Compute final pedal config
   let mSusPed = case pedSrcName of
         Nothing 
           | length pedAppNames > 0 -> 
               throwMine $ "in config, no part marked susped:source but " ++
               "one or more parts marked susped:apply"
           | otherwise -> Nothing
         Just srcName -> case pedConfigValues of
           Nothing ->
             throwMine $ "in config, some part marked susped:source but no "++
             "[susped ] statement is present"
           Just (liftDelta, liftDur) -> 
             Just $ SusPedConfig liftDelta liftDur srcName 
                                 (srcName:pedAppNames)

   -- Construct map of pattern statements
   let listPatternSt = [p | PcsPattern p <- entries]
       patternStNames = map pstName listPatternSt
   when (length (L.nub patternStNames) /= length patternStNames)
        (throwMine "in config file, two pattern statements with same name")
   let patMap = M.fromList $ map (pstName &&& id) listPatternSt


   -- timing variation
   let listTV = [x | PcsTimingVariation x <- entries]
       timingVar = case listTV of
         []  -> Nothing
         [x] -> Just x
         _   -> throwMine ("more than one timing-variation statement in " ++
                "config file")

   -- final value
   return $ PlConfFile staffMap ignoreNames mSusPed patMap timingVar

-}



