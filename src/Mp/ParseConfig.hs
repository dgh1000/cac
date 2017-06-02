{-

-- SAMPLE CONFIG --

Config consists of staff statements, pattern statements, sustain pedal config,
or comments:

  Comment starts with "[-"
    [- example of a comment]


  Pattern statement
  ------------------------------------------------------------

    A pattern statement, when present, indicates how to make dynamics and time
    alterations within measures and within sub-measures.

    For instance a 4/4 measure might have 4 sub-measures. (In that case, a
    sub-measure is the same as a "beat", but a measure need not have the same
    number of sub-measures as beats.)

    Each sub-measure is divided into sub-divisions.

    A pattern statement has the following form.

      [pattern "name" <... sub-measure  descriptions ...>
                      <... sub-division descriptions ...>
      ]

    "name" can be any quoted alphanumeric identifier.

    A "sub-measure description" begins with a time signature X, indicating it
    describes measures of time sig X. It indicates the number of submeasures
    per measure, and the number of subdivisions of each submeasure. It also
    gives the dynamic and tempo patterns for the submeasures.

      [3/4 submeasures:3 subdivisions:4 [dyn    0.0 -0.5 -0.75] 
                                        [tempo  1.1  0.9  0.90]
      ]

    A "Sub-division" description starts with a number X, meaning it refers to
    any submeasure that is specified to be divided into X subdivisions. It
    refers to any submeasure description in the pattern statement. That is, if
    Y is one of the submeasure descriptions inside the pattern statement, and
    Y has "subdivisions:X" specified, then the subdivision description with X
    will be used for Y.

      [4 [dyn    0.0 1.0 0.0 1.0] 
         [tempo  1.0 1.0 0.9 1.0]
      ]


    Note that any section within the pattern statement can be made into a
    comment with the dash '-'.


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


module Mp.ParseConfig where


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
import Util.Exception
import Mp.MpData
import Common.CommonData


parseIdentifier :: Parser String
parseIdentifier = many1 (alphaNum <|> char '_')

floatNum :: Parser Double
floatNum = do
  s <- many1 (oneOf "-0123456789.") 
  case reads s of
    (f,_):_ -> return f
    _       -> fail "invalid float number"


rationalNum :: Parser Rational
rationalNum  = do
  numerator <- many1 digit
  char '%'
  denominator <- many1 digit
  return $ (read numerator) % (read denominator)


intNum :: Parser Int
intNum = do
  s <- many1 (oneOf "0123456789")
  return $ read s


quotedString :: Parser String
quotedString = do
  char '"'
  n <- many1 (alphaNum <|> oneOf "- ()[]")
  char '"'
  return n



----------------------------------------------------------------------
----------------------------------------------------------------------
--                       pattern 


pattern :: Parser ParsedConfSt
pattern = do
  try $ string "[pattern"
  fail "not currently supporting patterns"
  skipMany space
  name <- quotedString
  skipMany space
  ss <- many $ subMeas <|> subDiv
  many space
  char ']'
  many space

  -- Construct map of submeasure sections
  let subMeasSections = [sm | PspSubMeas sm <- ss]
      timeSigs = map psmTimeSig subMeasSections
  when (length (L.nub timeSigs) /= length timeSigs) 
       (throwMine $ printf ("two submeasure sectons in pattern '%s' have " ++
                            "same time sig") name)
  let subMeasMap = M.fromList $ map (psmTimeSig &&& id) subMeasSections

  -- Construct map of subdivision sections
  let subDivSections = [sd | PspSubDiv sd <- ss]
      ns = [n | PatternSubDiv n _ <- subDivSections]
  when (length (L.nub ns) /= length ns)
       (throwMine $ printf ("two subdivision sections in pattern '%s' have "++
                            "same N") name)
  let subDivMap = M.fromList $ map (psdN &&& id) subDivSections
  return $ PcsPattern $ PatternSt name subMeasMap subDivMap


-- Validation utilities
numErr typ n       = fail $ printf ("expected %d entries in '%s' "++
                     "subsection") n typ
noSectErr      typ = fail $ printf "no %s subsections" typ
tooManySectErr typ = fail $ printf "too many %s subsections" typ


dynsTempos :: Int -> Parser [(Double,Double)]
dynsTempos n = do
  pls <- many patList
  -- validate pattern lists
  dyns   <- case [ds | PplDyn ds <- pls] of
              []  -> noSectErr "dyn"
              [xs] | length xs /= n -> numErr "dyn" n
                   | otherwise      -> return xs
              _   -> tooManySectErr "dyn"
  tempos <- case [ts | PplTempo ts <- pls] of
              []   -> noSectErr "tempo"
              [xs] | length xs /= n -> numErr "tempo" n
                   | otherwise      -> return xs
              _    -> tooManySectErr "tempo"
  return $ zip dyns tempos


patList :: Parser ParsedPatList
patList = do
  char '['
  ((try $ do string "dyn"
             many1 space
             xs <- manyDouble
             return $ PplDyn xs)
   <|>
   (try $ do string "tempo"
             many1 space
             xs <- manyDouble
             return $ PplTempo xs))
  

manyDouble :: Parser [Double]
manyDouble = do
  let f = do x <- floatNum
             many space
             return x
  xs <- many f
  char ']'
  many space
  return $ xs


subMeas :: Parser ParsedSubPat
subMeas = do
  (n,d) <- try $ do char '['
                    n <- intNum
                    char '/'
                    d <- intNum
                    return (n,d)
  skipMany space
  string "submeasures:"
  sm <- intNum
  skipMany space
  string "subdivisions:"
  sd <- intNum
  skipMany space
  dts <- dynsTempos sm
  char ']'
  skipMany space
  return $ PspSubMeas $ PatternSubMeas (n,d) sm sd dts
  

subDiv :: Parser ParsedSubPat
subDiv = do
  n <- try $ do char '['
                x <- intNum
                many1 space
                return x
  dts <- dynsTempos n
  char ']'
  skipMany space
  return $ PspSubDiv $ PatternSubDiv n dts



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
--            code for converting a list of PatternEntry into
--                a more organized PatterDescr


{-
toPatternDescr :: [PatternEntry] -> PatternDescr
toPatternDescr pes = PatternDescr dtMap ts
  where
    ts = [((num,den),len) | PETimeSig num den len <- pes ]
    dtMap = M.fromList $ mapMaybe dynTempoOfN [1..12]
    -- Given 'n', the length of a pattern, look for [dynamics] and [tempo]
    -- statements of that length. If none exist, return None. If both exist
    -- for 'n', return Just [(<dyn alteration>, <tempo alteration>)]. If one
    -- exists but not the other, throw an error.
    dynTempoOfN :: Int -> Maybe (Int,[(Double,Double)])
    dynTempoOfN n = case (dynAlts,tempoAlts) of
        (ds:_,ts:_) -> Just (n,zip ds ts)
        ([],[])     -> Nothing
        _           -> throwMine $ printf ("problem in looking up dynamic " ++
                       "and tempo patterns of length %d in the config file" ++
                       " 'pattern' statement: either dyn or tempo exists, " ++
                       "but not both") n
      where
        dynAlts   = [alts | PEDynamics len alts <- pes, len==n]
        tempoAlts = [alts | PETempo    len alts <- pes, len==n]
-}


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


-- computes <document name>, <Part instrument name>, <map of values>
staff :: Parser ParsedConfSt
staff = do
  try $ string "[staff"
  notFollowedBy $ char '-'
  skipMany space
  xmlStaffName <- quotedString
  skipMany space
  string "instr:"
  staffInstrName <- many1 (alphaNum <|> char '_')
  skipMany space
  opts <- many (susPedUse <|> muteState)
  midiDests <- M.fromList <$> many1 staffMidi
  fs <- M.fromList <$> many staffField
  char ']'
  skipMany space
  return $ PcsStaff $ StaffConfig xmlStaffName staffInstrName 
           (computeMuteState opts) midiDests fs (computeSusPedUse opts)


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




