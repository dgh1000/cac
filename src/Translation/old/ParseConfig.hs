module Translation.ParseConfig where


import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Arrow
import Text.Parsec
import Text.Parsec.ByteString
import Text.Printf
import Data.Maybe
import Data.Map(Map)
import Data.Ratio
import Data.Set(Set)
import Translation.TranslationData
import Common.CommonExport
import Util.Exception
import Util.Showable


-- problem: it is simplest to write code to parse specific structures rather
-- than parse anything and validate it later. but then we have to have unique
-- representation of

-- want recursve structure


{-

-- SAMPLE CONFIG --

Config consists of staff statements, pattern statements, sustain pedal config,
or comments:


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


  Staff statement
  ------------------------------------------------------------

    [staff <quoted staff name> instr:<non-quoted instr name>
           <staff-options>
           <midi statement>
           <parameters>
    ]

  Staff options: zero or more of

     susped:<option>   where <option> is 'apply' or 'source'

     mute:<option>     where <option> is 'all' or 'controllers' or 'notes'

  Midi statement:

     [midi <local id> stream:<stream #> chan:<channel #>]

     In 'midi' statement, first argument is the channel id used by that
     particular instrument to identify a midi channel for a specific
     purpose. For instance, maybe the pizz channel in a Quantum Leap
     instrument, in which case the id is 'pizz'. The channel id is not quoted.

  Parameters:

     <param name>:<value>

     where value can be an integer, floating-point number, or alphanumeric
     word. If you put all digits, it will be interpreted as an integer. if
     is contains all digits plus one period, it will be interpreted as a 
     floating point number. If it contains at least one non-numeric character,
     it will be interpreted as alphanumeric


  staff-ignore statement
  ----------------------------------------------------------------------

    [staff-ignore <quoted staff name> ]

    This means to ignore any notes in this staff. Don't translate to MIDI.

  timing variation statement
  ----------------------------------------------------------------------

    [timing-variation minLen:<int>    maxLen:<int> 
                      ratio1:<double> ratio2:<double>
                      delta1:<double> delta2:<double> ]

    See /haskell/Translation/TimeMap.hs for algorithm


  moog-chans
  ----------------------------------------------------------------------

  [moog-chans]


  shape
  ----------------------------------------------------------------------

  maybe should call it "moog" .. because better not to generalize too much,
  easier to make it make sense if it's specific to the situation

  [shape

    foo1(id)

  ]




[- comment ]
[staff "Organ-staff1"  instr:gOrgan  mute:controllers

  [midi 1 stream:0 chan:1]
  vol:127]
[staff "Organ-staff2"  instr:gOrgan susped:source 
  [midi 1 stream:0 chan:2]
  vol:127]
[staff "Pedals-staff1" instr:gOrgan susped:apply 
  [midi 1 stream:0 chan:3]
  vol:127]

[susped ????]



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


identifier = do
  c <- letter
  cs <- many $ alphaNum <|> char '_'
  return $ c:cs


quoted :: Parser String
quoted = do
  char '"'
  manyTill anyChar (char '"')


intNum :: Parser Int
intNum = try $ do
  s <- many1 digit
  notFollowedBy $ char '.'
  return $ read s


floatNum :: Parser Double
floatNum = try $ do
  c <- option "" (string "-")
  n1 <- many1 digit
  char '.'
  n2 <- many1 digit
  notFollowedBy $ char '.'
  return . read $ c ++ n1 ++ "." ++ n2


configValue :: Parser ConfigValue
configValue = ConfigValueInt <$> intNum
              <|>
              ConfigValueDouble <$> floatNum
              <|>
              ConfigValueString <$> identifier
              <|>
              ConfigValueString <$> quoted


param :: Parser Param
param = do
  i <- identifier
  char ':'
  v <- configValue
  return $ Param i v



{-

single :: Parser Elem
single = try $ do
  s <- many1 alphaNum
  char ':'
  v <- value
  return $ ESingle s v


bracketed :: Parser Elem
bracketed = do
  char '['
  s <- many1 alphaNum
  many1 space
  es <- many1 $ do {e <- element; many space; return e}
  char ']'
  return $ EBracketed s es





matchesSingle :: String -> Elem -> Maybe Value
matchesSingle _ (EBracketed _ _) = Nothing
matchesSingle name (ESingle s v) | name == s = Just v
                                 | otherwise = Nothing


findSingle :: String -> [Elem] -> Parser Value
findSingle name es = case mapMaybe (matchesSingle name) es of
  []  -> fail $ printf "no instance of element '%s'" name
  [v] -> return v
  _   -> fail $ printf "multiple instances of element '%s'" name


findSingleString :: String -> [Elem] -> Parser String
findSingleString name es = do
  v <- findSingle name es
  case v of
    VString s -> return s
    _         -> fail "element '%s' is not a string"


findSingleList :: String -> [Elem] -> Parser [Value]
findSingleList name es = do
  v <- findSingle name es
  case v of
    VList vs -> return vs
    _        -> fail "element '%s' is not a list"


toString :: Value -> Parser String
toString (VString s) = return s
toString _           = fail "expected string"


findStaffNames :: [Elem] -> Parser [String]
findStaffNames es = do
  vs <- findSingleList "staves" es
  mapM toString vs

-}

meta :: Parser ConfigEntry
meta = do
  try $ char '[' >> string "meta" >> many1 space
  string "instr:"
  instrType <- identifier
  many1 space
  string "name:"
  name <- identifier
  many1 space
  string "[staves"
  many1 space
  staves <- many1 $ do {q <- quoted; many space; return q}
  char ']'
  many1 space
  ps <- many $ do {p <- param; many space; return p}
  char ']'
  many space
  return $ MetaInstrC instrType name staves ps




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



