{-# LANGUAGE FlexibleContexts #-}


module Score.ParseWords where


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Debug.Trace
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
import Common.CommonUtil(locAdd,locSub,locDiff, simpleShowLoc,showLoc2)
import Common.CommonData
import Util.Exception
import Util.Math(scale)
import XmlDoc.XmlDocData

{-


rethink how durations and numbers are specified

  we find it necessary to use variables as part of specifying a
  duration. right now we can only use those for tempos

  what kinds of numbers are specified right now?

    durations

      in what commands do these appear?

      @ to indicate 10s of milliseconds

      % to indicate ratio that gives number of beats

        ps: can use either form

        warps: must use % or integer

        arp, stac: must use @


      in some cases we want to use an integer or float to specify number of
      beats with no need for special character

        does that mean 

    tempos and tempo ratios

      right now, the only implementation of variables

      in what commands do these appear?

        T= (can use variables)

        ramps (can't use variables)

we want anything to use variables

  do we need "tempo variables" and "duration variables"? both of them can be
  "number of beats" or "number of quarters". it's just that with tempo we
  implicitly mean adjust the duration to fit that many in a minute.

  use any variable in any command, then? 

    might want to still use an explicit ratio form in ramps to indicate ratio
    of prevailing tempo. oh can use that in T= as well.




-}



{-

tempos in setting tempo or variable

  T=3:2*

    set tempo to 3/2 of the prevailing tempo

  T=100Q

    set tempo to 100 bpm

  T=a

    set tempo to variable a

  T=9:10a

    set tempo to 9/10 of a

  a=90Q

    set a to 90 quarters/minute

  a=3:2b   

    set variable a to 3/2 variable b


ramps

  Update June 2016: simplifying: ramp tempos will always be ratios with
  respect to prevaling tempo. Also ratios can be specified by omitting either
  number and the missing number is considered to be 1. Or a single int X means
  X:1. And a pipe divides the ratios at a sudden tempo change.

  Also update June 2016: changing ramp indicator to two dashes, not greater
  than/ less than symbols.

    (Legacy form supported for now.)

  In the following we call the prevailing tempo P

  3:2--    --1:2

    ramp from 3/2 P to 1/2 P. return to P afterward.

  1--    --1:2=

    ramp from P to  1/2 P and continue at 1/2P

  1--   --2--    --4:5

    ramp from P to 2 P, then back to 4/5 P, and return to P

  1--  --1:2|4:5--  --1

    ramp from P to 1/2 P, then sudden change to 4:5 P, ramping back up to P

warps

  note: in warps with interval width specified, it can be an integer (not
  necessarily a ratio) -- however the amount of the warp has to be a ratio

  <1;%10 

     global warp because it has no S. Left-sided warp because < is on the
     left. warp interval of 1 beat because "1" is before the ";". Compresses
     interval because < is pointing in same direction as side it is on. Total
     compression amount is 1/10 of a beat.

  <+%10<

     staff-only (because of the +), double-sided warp because there are '<' on
     each side. There is only one number inside the warp, so that number is
     the amount to compress left interval and stretch right interval, while
     look for w's to mark ends of interval

  <+1;%10<

     staff-only double-sided, left and right intervals are each 1 beat in
     duration

  <+1;%10

     staff-only single-sided. This will cause staves to get out of sync until
     the next one sided warp staff-only warp in the same warp that restores
     sync


  >%10> 

     global double-sided, stretch left interval and compress right interval by
     1/10 of a beat. look for w's to define intervals.

  3%2;%8>

     global right-sided warp. The arrow pointing right means compress the
     interval. This doesn't actually affect the arrival time at the location
     the warp is marked but rather makes the next 3/2 of a beat speed up by a
     total amount of 1/8 a beat.

  warp to specific final duration, not a change in duration

    


pause

  ps5 : NOT SUPPORTED ANY MORE

    legacy form: pause for 50 milliseconds

  ps@5

    new form: pause for 50 milliseconds

  ps%8

    pause for 1/8 of a beat

  ps2%7

    pause for 2/7 of a beat


arpeggio and staccato durations

  arp10 : NOT SUPPORTED ANY MORE

    legacy form: set arp delta to 10*10 milliseconds

  stac5 : NOT SUPPORTED ANY MORE

    legacy form: set staccato duration to 5*10 milliseconds

  arp@5, stac@7

    updated form of specifying 10's of milliseconds


rit/accel

  either "rit." or "accel."

    either of these specifies a ramp from prevailing tempo to next marked
    tempo. it treats "rit." and "accel." the same; the next marked tempo
    determines whether the tempo will speed up or slow down


splice

  $a

    mark beginning of sequence of segments, each one marked by a letter such
    as $b, $c etc. 


trill/tremolo

  there is a trill scale expressing minimum to maximum trill rate, in which 0
  is minimum (perhaps 4 notes/sec) and 9 is maximum (perhaps 15 notes/sec)

  t^25-93v

    next trill will start at rate of 2, continue for 5 units accelerating
    toward rate of 9, then continue for 3 units. Total duration is 8 units. A
    unit in absolute terms will be 1/8 of the duration of the trill. Trill
    will start on upper note and end on lower note.

  t^25-93^

    same as above but trill will start AND end on upper note 

  tv25-95-00^

    trill will start on lower note. rate 2, ramp for duration 5 units to 9,
    ramp for 5 units to 0, stay at 0 for 0 units. duration of unit is 1/10
    total length of trill. trill will end on upper note.

  trates:4,15

    set trill rate scale. The first number, 4, will be the rate of any trill
    marked as rate 0. The last number, 15 will be rate of a trill marked as
    rate 9. In-between rates are scaled linearally.


patterns (tempo and dynamics patterns)

  pat:<name>

    apply rhythmic and dynamics patterns from pattern statement in the config
    with name <name> between this mark and the next pat: mark or the end of
    the document


macros: 

  definition:

    name must start with letter, followed by 0 or more alphaNum

    arguments are specified by /<digit> and must be numbered consecutively

    example:

      wl{<+%2;%/1<}

  macro invocation: give name and arguments separated by !

    wl!12

-}


----------------------------------------------------------------------
----------------------------------------------------------------------
--        testing help


wordTesting :: Map Int TimeSig -> Map Loc [String] -> Map Loc [Mark]
wordTesting timeSigs words = lMapMap doWarp markWords
  where
    isW P1W = True
    isW _   = False
    pass1Words = lMapMap toPass1Word words
    macroDefns  = combineMacroDefns [pass1Words]    
    wLocs = S.fromList . map fst . filter (\(_,p) -> isW p) . lMapToList $
            pass1Words
    markWords = toMarkWords macroDefns pass1Words
    doWarp loc w@MwWarpTemp{} = toWarp w timeSigs loc wLocs
    doWarp _   (MwMark m)     = m


----------------------------------------------------------------------
----------------------------------------------------------------------
--                 pass 1 functions

toPass1Word :: Loc -> String -> Pass1Word
toPass1Word loc s = case parse pass1Word "" s of
  Left err -> throwMine $ printf "XML Words parse error at loc %s:\n%s"
                                 (showLoc2 loc) (show err)
  Right w  -> w


pass1Word = macroDefn <|> macroInstance <|> comment <|> w <|>
            (P1Normal <$> many1 anyChar)


macroDefn :: Parser Pass1Word
macroDefn = do
  name <- try $ do name <- macroName
                   char '{'
                   return name
  -- If we succeed to here, it cannot match another parser
  cs <- manyTill macroDefnChar (char '}')
  eof
  -- validate that arguments are in order from 1 to n
  let argNumbers = L.sort [n | MdcArg n <- cs]
  when (argNumbers /= [1..length argNumbers])
       (fail $ "in macro definition, arguments are not numbered " ++
               "consecutively from one")
  return $ P1MacroDefn $ MacroDefn name (length argNumbers) cs
  

macroName = do
  c  <- letter
  cs <- many alphaNum
  return $ c:cs


macroDefnChar :: Parser MacroDefnChar
macroDefnChar =
  (do char '/'
      c <- digit <?> "expecting digit after backslash"
      return $ MdcArg (read [c]))
  <|>
  (MdcChar <$> anyChar)


-- if the word is neither "rit." nor "accel." and there are any periods
-- anywhere, then no other parser will match and we can go ahead and parse
-- this as MacroInstance
macroInstance :: Parser Pass1Word
macroInstance = do
  name <- try $ do name <- macroName
                   char '!'
  fields <- sepBy1 (many1 $ noneOf "!") (char '!')
  eof
  return $ P1MacroInstance $ MacroInstance name fields


-- if 'w' matches that is not guarantee no other parser will match, so must
-- not consume any input if failure.
w :: Parser Pass1Word
w = try $ do char 'w'
             eof
             return P1W


-- no other parser looks for '*' at the beginning, so this is fine not to use
-- try.
comment :: Parser Pass1Word
comment = string "**" >> return P1Comment




----------------------------------------------------------------------
----------------------------------------------------------------------
--
--   Converting Pass1Word to MarkWord. This involves applying macros and doing
--   most parsing.


combineMacroDefns :: [Map Loc [Pass1Word]] -> Map String (Loc,MacroDefn)
combineMacroDefns mapsIn = foldl unionMacroDefns M.empty (map g mapsIn)
  where
    unionMacroDefns :: Map String (Loc,MacroDefn) -> 
                       Map String (Loc,MacroDefn) ->
                       Map String (Loc,MacroDefn)
    unionMacroDefns x unioned = M.unionWithKey err x unioned
      where
        err name (loc1,_) (loc2,_) = throwMine $ printf ("two macro defns " ++
                                     "by same name '%s', at %s and %s")
                                     name (showLoc2 loc1) (showLoc2 loc2)
    g :: Map Loc [Pass1Word] -> Map String (Loc,MacroDefn)
    g words = M.fromListWithKey err . mapMaybe h . lMapToList $ words
      where
        h :: (Loc,Pass1Word) -> Maybe (String,(Loc,MacroDefn))
        h (loc,P1MacroDefn d@(MacroDefn name _ _)) = Just (name,(loc,d))
        h _                              = Nothing
        err name (loc1,_) (loc2,_) = 
          throwMine $ printf ("two macros with same name '%s', at locs %s "++
          "and %s") name (showLoc2 loc1) (showLoc2 loc2)


toMarkWords :: Map String (Loc,MacroDefn) -> Map Loc [Pass1Word] -> 
               Map Loc [MarkWord]
toMarkWords macros = lMapMaybeWithKey g
  where
    g :: Loc -> Pass1Word -> Maybe MarkWord
    g loc (P1Normal s)   = case parse markWord "" s of
      Left err -> throwMine $ printf "XML word parse error at %s: %s"
                  (showLoc2 loc) (show err)
      Right m  -> Just m
    g instanceLoc (P1MacroInstance (MacroInstance nameToApply args)) = 
      case M.lookup nameToApply macros of
        Just (defnLoc,defn) -> Just $ applyMacro defnLoc defn instanceLoc args
        Nothing             -> throwMine $ printf ("macro instance at %s. "++
                               "no macro definition named '%s'")
                               (showLoc2 instanceLoc) nameToApply
    g _ _ = Nothing
      


applyMacro :: Loc -> MacroDefn -> Loc -> [String] -> MarkWord
applyMacro macroLoc (MacroDefn _ nArgs chars) instanceLoc args
  | nArgs /= length args = throwMine $ printf ("in macro instance at %s, "++
                           "number of args is %d. This does not match " ++
                           "definition at loc %s.") 
                           (showLoc2 instanceLoc) (length args) 
                           (showLoc2 macroLoc)
  | otherwise = case parse markWord "" (concatMap g chars) of
                  Left err -> throwMine $ printf ("in parsing macro "++
                              "instance after subtitution: %s") (show err)
                  Right w  -> w
  where
    g (MdcChar c) = [c]
    g (MdcArg  i) = args !! (i-1)


----------------------------------------------------------------------
----------------------------------------------------------------------
--   Parsers for MarkWord


markWord = 
  setTempo <|> setVar  <|> splice    <|> arp             <|> stac    <|>
  warp     <|> pause   <|> rampBegin <|> rampEnd         <|> 
  rit      <|> accel   <|> pat       <|> trillShapeMark


setTempo = do
  try $ string "T="
  t <- tempo
  eof
  return $ MwMark $ SetTempo t


setVar = do
  v <- try $ do s <- varName
                char '='
                return s
  t <- tempo
  eof
  return $ MwMark $ SetVar v t


trillShapeMark = do
  step1 <- try $ do string "tr"
                    choice [char '^',char 'v']
  t <- trillShape step1
  eof
  return $ MwMark $ TrillShapeMark t


tremShapeMark = do
  step1 <- try $ do string "to"
                    choice [char '^', char 'v']
  t <- trillShape step1
  eof
  return $ MwMark $ TremShapeMark t


splice = do
  char '$'
  c <- oneOf ['a'..'z']
  eof
  return $ MwMark $ SpliceMark c


{-
simpleRampBegin = do
  try (string ">>" >> eof)
  return $ MwMark $ RampBeg (NumRatio 1 1)
-}


rampBegMarker = (try $ string ">>") <|> (try $ string "--")


rampEndMarker = (try $ string "<<") <|> (try $ string "--")


rampBegin = do
  lookAhead $ try $ do many1 (notFollowedBy rampBegMarker >> anyChar)
                       rampBegMarker
                       eof
  r <- numRatioPlusInt
  rampBegMarker
  eof
  return $ MwMark $ RampBeg r


rampEnd = do
  try rampEndMarker
  r1 <- numRatioPlusInt
  (endBegResult,continueFlag) <- endBeg_continue_neither
  case (endBegResult,continueFlag) of
    (Just (Just r2), _   ) -> return $ MwMark $ RampEndBeg r1 r2
    (Just Nothing  , _   ) -> return $ MwMark $ RampEndBeg r1 r1
    (_             , flag) -> return $ MwMark $ RampEnd r1 flag


endBeg_continue_neither = do
  -- This is case of an End/Beg that has split ratios
  (do char '|'
      r <- numRatioPlusInt
      rampBegMarker
      eof
      return (Just (Just r),False))
  <|>
  -- This is case of an End/Beg with no second ratio
  (do rampBegMarker
      eof
      return (Just Nothing,False))
  <|>
  -- This is the case of a simple ramp end.
  (do eof
      return (Nothing,False))
  <|>
  -- This is case of ramp end with continue flag
  (do char '='
      eof
      return (Nothing,True))
     
  
warp = do
  (try $ do char '<'
            warpFollow (-1))
  <|>
  (try $ do char '>'
            warpFollow 1)
  <|>
  (do lookAhead $ try $ manyTill anyChar endsLikeWarp
      warpFollow 0)


endsLikeWarp :: Parser ()
endsLikeWarp = (char '>' <|> char '<') >> eof


warpFollow :: Int -> Parser MarkWord
warpFollow leftDir = do
  glob        <- globalFlag
  (width,amt) <- warpInside
  rightDir    <- warpArrow
  eof
  return $ MwWarpTemp leftDir rightDir width amt glob


globalFlag = option True (char '+' >> return False)


warpArrow :: Parser Int
warpArrow = (char '<' >> return (-1)) <|> (char '>' >> return 1) <|> return 0


warpInside :: Parser (Maybe Rational,Rational)
warpInside = do
  r1 <- rationalPlusInt
  ((do char ';'
       r2 <- rationalPlusInt
       return (Just r1,r2))
   <|>
   return (Nothing,r1))


pause = do
  try $ string "ps"
  d <- duration
  eof
  return $ MwMark $ Pause d


arp = do
  try $ string "arp"
  char '@'
  mult <- option 1 (char '-' >> return (-1))
  i <- parseInt
  eof
  return $ MwMark $ ArpDelta $ 0.01*mult*fromIntegral i


stac = do
  try $ string "stac"
  s <- seconds
  eof
  return $ MwMark $ StacDur s


rit = do
  try $ do string "rit."
           eof
  return $ MwMark RitAccel


accel = do
  try $ string "accel."
  return $ MwMark RitAccel


pat = do
  try $ string "pat:"
  (MwMark . PatternMark) <$> many1 anyChar


trillShape :: Char -> Parser TrillShape
trillShape step1 = do
  let seg = do d1 <- digit
               d2 <- digit
               let d = fromIntegral $ read [d1]
               return (scale 0 d 9 4 15,read [d2])
  segs <- sepBy seg (char '-')
  when (null segs) (fail "must be at least one segment in trill/trem shape")
  step2 <- choice [char '^', char 'v']
  let step11 = if step1 == '^' then Upper else Lower
  let step22 = if step2 == '^' then Upper else Lower
  return $ TrillShape step11 segs step22

 


{-
-- xmlWordsToMarks
--
-- Every XML word-type text string is converted to a ParsedWord.
--
-- Those are converted to a map of Marks.
--
xmlWordsToMarks :: Map Int TimeSig -> Map Loc [XMsrData] -> Map Loc [Mark]
xmlWordsToMarks timeSigs msrData = 
    xmlWordsToMarks_help timeSigs words
  where
    -- Make an L-Map of Loc to word strings.
    words  :: Map Loc [String]
    words  = lMapMaybe maybeWords msrData


xmlWordsToMarks_help :: Map Int TimeSig -> Map Loc [String] -> Map Loc [Mark]
xmlWordsToMarks_help timeSigs words = listToLMap $ mostMarks ++ warps
  where
    -- parsedWords: list of ParsedWord data. This is first pass, which pulls
    -- out macros, comments, and w's.
    parsedWords :: [(Loc,ParsedWord)]
    parsedWords = map toParsedWord . lMapToList $ words

    -- extract the w locations
    wLocs = S.fromList [loc | (loc,PwW) <- parsedWords]

    -- toPostMacroWord: looks at ParsedWords and applies macros to get
    -- everything to simple strings, ready for parsing as "normal words"
    postMacroWords :: [(Loc,PostMacroWord)]
    postMacroWords = toPostMacroWords parsedWords

    -- toNormalWords: does parsing of strings in post-macros words to come up
    -- with Marks (of the non-warp variety) and also temp warp data.
    markWords :: [(Loc,MarkWord)]
    markWords = map toMarkWord postMacroWords

    -- extract Marks (embedded in MwMark constr)
    mostMarks = [(loc,m) | (loc,MwMark m) <- markWords]

    -- convert PwWarpTemp to Mark (of constructor Warp)
    warps :: [(Loc,Mark)]
    warps = map (fromWarpTemp timeSigs wLocs)
                [(loc,pw) | (loc,pw@MwWarpTemp{}) <- markWords]
-}

{-
----------------------------------------------------------------------
----------------------------------------------------------------------
--                  convert XML word strings to ParsedWord


toParsedWord :: (Loc,String) -> (Loc,ParsedWord)
toParsedWord (loc,s) = case parse word "" s of
  Left err -> throwMine $ printf "XML Words parse error at loc %s:\n%s"
                                 (showLoc2 loc) (show err)
  Right w  -> (loc,w)

-}
----------------------------------------------------------------------
----------------------------------------------------------------------
--         apply macros: convert 
{-




toPostMacroWords :: [(Loc,ParsedWord)] -> [(Loc,PostMacroWord)]
toPostMacroWords parsedWords = mapMaybe doOne parsedWords
  where
    -- Pull out all macro definitions
    macroDefns = validateMacroDefns
                 [(loc,d) | (loc,PwMacroDefn d) <- parsedWords]

    -- Process one parsed word
    doOne (loc,p) = case p of
      PwMacroInstance name args -> Just $ applyMacro macroDefns loc name args
      PwNormal s                -> Just $ (loc,PostMacroWord Nothing s)
      _                         -> Nothing

-}

{-
-- Validate that each MacroDefn has a proper argument list and that every
-- macro has a unique name.
validateMacroDefns :: [(Loc,MacroDefn)] -> [(Loc,MacroDefn)]
validateMacroDefns ms 
  | length (L.nub names) /= length names 
      = throwMine $ printf "(in ParseWords.hs) two macros have the same name"
  | otherwise = ms
  where
    names = map g ms
    g :: (Loc,MacroDefn) -> String
    g (_,MacroDefn name _ _) = name
-}


{-
validateMacroDefnArgs :: (Loc,MacroDefn) -> (Loc,MacroDefn)
validateMacroDefnArgs inp@(loc,MacroDef _ cs)
  | argNumbers == [1 .. numArgs] = inp
  | otherwise = throwMine $ printf ("in macro definition at %s, arg numbers"++
                "are %s, which is not a consecutive list starting with 1")
                (showLoc2 loc) argNumbers
  where
    argNumbers = L.sort [i | MdcArg i <- cs]
    numArgs = length argNumbers
-}

{-
  where
    g (MdcChar c) = [c]
    g (MdcArg  i) = args !! (i-1)
    (nArgs,defnChars,defnLoc) =
        case [(nArgs,defn,defnLoc) |
               (defnLoc,MacroDefn name nArgs defn) <- macroDefns, 
               name == macroNameToApply
             ] of
      []    -> throwMine $ printf ("at macro invocation at %s, cannot find "++
               "macro definition with name '%s'") (showLoc2 instanceLoc)
               macroNameToApply
      _:_:_ -> throwMine $ printf ("at macro invocation at %s, found more "++
               "than one macro definition with name '%s'") 
               (showLoc2 instanceLoc) macroNameToApply
      x:_   -> x
-}



----------------------------------------------------------------------
----------------------------------------------------------------------
--                convert from PostMacroWord to MarkWord

{-
toMarkWord :: (Loc,PostMacroWord) -> (Loc,MarkWord)
toMarkWord (loc,PostMacroWord mMacroInfo s) = case parse markWord "" s of
  Left err -> throwMine $ printf "text parse error at %s %s: %s" (showLoc2 loc)
                          macroMsg (show err)
  Right n  -> (loc,n)
  where
    macroMsg = case mMacroInfo of
      Nothing                   -> ""
      Just (macroLoc,macroName) -> printf ("(this word came from expanding "++
                                   "macro at %s, of name '%s'") 
                                   (showLoc2 macroLoc) macroName


-}

----------------------------------------------------------------------
----------------------------------------------------------------------


markWordsToWords :: Map Int TimeSig -> Set Loc -> Map Loc [MarkWord] ->
                    Map Loc [Mark]
markWordsToWords timeSigs wLocs = M.mapWithKey g
  where
    g :: Loc -> [MarkWord] -> [Mark]
    g loc = map h
      where
        h :: MarkWord -> Mark
        h (MwMark m) = m
        h t@MwWarpTemp{} = toWarp t timeSigs loc wLocs


{-
fromWarpTemp :: Map Int TimeSig -> Set Loc -> (Loc,MarkWord) -> (Loc,Mark)
fromWarpTemp timeSigs wLocs (loc,warpTemp) = 
  (loc,toWarp warpTemp timeSigs loc wLocs)
-}
  

toWarp :: MarkWord -> Map Int TimeSig -> Loc -> Set Loc -> Mark
toWarp (MwWarpTemp leftDir rightDir width amt globalFlag) timeSigs loc wMarks
  = Warp globalFlag leftLoc rightLoc amt (fromIntegral direction)
  where
    msg1 = printf "at loc %s, warp with no arrow on either side" 
           (simpleShowLoc loc)
    msg2 = printf ("at loc %s, warp with arrows on each side pointing in " ++
           "different directions") (simpleShowLoc loc)
    msg3 s = printf ("no %s W at warp at %s") s (simpleShowLoc loc)
    msg4   = printf ("left interval prior to composition start at warp " ++
             "at %s") (simpleShowLoc loc)
    msg5   = printf ("right interval after composition end at warp " ++
             "at %s") (simpleShowLoc loc)
    (direction,leftLoc,rightLoc) = case (leftDir,rightDir) of
      (0,  0)       -> throwMine $ msg1
      (d,  0)       -> (d,  computeLeftLoc,         Nothing)
      (0,  d)       -> (d,         Nothing, computeRightLoc)
      (d1,d2) 
        | d1 /= d2  -> throwMine $ msg2
        | otherwise -> (d1, computeLeftLoc, computeRightLoc)
    computeLeftLoc = case width of
      Nothing -> case S.lookupLT loc wMarks of
        Nothing -> throwMine $ msg3 "left"
        Just l  -> warpWarning timeSigs loc l
      Just w  -> case locSub timeSigs loc w of
        Nothing -> throwMine msg4
        Just l  -> Just l
    computeRightLoc = case width of
      Nothing -> case S.lookupGT loc wMarks of
        Nothing -> throwMine $ msg3 "right"
        Just l  -> warpWarning timeSigs loc l
      Just w  -> case locAdd timeSigs loc w of
        Nothing -> throwMine $ msg5
        Just l  -> Just l


-- warpWarning
--
-- l1:: the loc at which the warp mark is located
-- l2:: the loc at which the adjacent w is located
--
-- In all cases, returns 'Just l2', but also gives a trace warning if
-- l2 is >=8 beats away from l1.
warpWarning timeSigs l1 l2
  | d >= 8 = printf "---WARNING--- w is pretty far away from warp at %s" 
             (simpleShowLoc l1) `trace` Just l2
  | otherwise = Just l2
  where
    d = if l1 < l2 
         then locDiff timeSigs l1 l2 
         else locDiff timeSigs l2 l1



----------------------------------------------------------------------
----------------------------------------------------------------------
--  General parsers.

seconds = do 
  char '@'
  i <- parseInt
  return $ 0.01 * fromIntegral i


-- For absolute tempos, can only handle quarters per minute right now 
--
-- 100 Q
-- a
-- 3:2a
-- 3:2
tempo :: Parser Tempo
tempo = tempoVarNameOnly <|> tempoRelative <|> tempoAbs


tempoVarNameOnly = do
  v <- varName 
  return $ TempoRelative (NumRatio 1 1) (Just v)


tempoRelative = do
  r <- try numRatio
  v <- optionMaybe varName
  return $ TempoRelative r v


tempoAbs = do
  t <- try parseInt
  char 'Q'
  return $ TempoAbs t
  

rational :: Parser Rational
rational =
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


rationalPlusInt :: Parser Rational
rationalPlusInt =
  (try $ do i <- parseInteger
            notFollowedBy (char '%')
            return $ i%1)
  <|> 
  rational


simpleDouble :: Parser Double
simpleDouble = do
  s <- many1 (oneOf ".0123456789")
  case reads s of
    (f,[]):_ -> return f
    _        -> fail "malformed numeric value"


varName :: Parser String
varName = oneOf ['a'..'z'] >>= \c -> return [c]


numRatio :: Parser NumRatio
numRatio =
  (try $ do n1 <- simpleDouble
            char ':'
            notFollowedBy digit
            return $ NumRatio n1 1)
  <|>
  (try $ do char ':'
            n2 <- simpleDouble
            return $ NumRatio 1 n2)
  <|>
  (try $ do n1 <- simpleDouble
            char ':'
            n2 <- simpleDouble
            return $ NumRatio n1 n2)


numRatioPlusInt :: Parser NumRatio
numRatioPlusInt = 
  (try $ do x <- simpleDouble
            notFollowedBy (char ':')
            return $ NumRatio x 1)
  <|>
  numRatio


duration :: Parser Duration
duration = DurSecs <$> (try seconds) <|> DurBeats <$> (try rational)


{-

macros can be on different staves

  a new wrinkle with macros. we must have words from all staves in order to
  fully convert them to marks.


-}
