{-# LANGUAGE FlexibleContexts,TupleSections #-}


module Score.ParseMarks where


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

  *** we no longer allow setting tempo in terms of prevailing tempo except in
      ramps. you can only give a number, or a number times a variable, in the
      T= form ***

  *** Q is no longer necessary. it is assumed that all tempo specifications
      are in number of quarters per minute. Later maybe we can add some other
      beat type ***


  T=100  (Q is no longer necessary)

    set tempo to 100 quarters per minute

  T=a

    set tempo to number of beats in variable a

  T=9%10a

    set tempo to 9/10 of a


variables to indicate a number of beats

  a=90

    set a to 90 quarters

  a=3%2b

    set variable a to 3/2 variable b

  a=3%2

    set a to 3/2 quarters

ramps

  Update June 2016: simplifying: ramp tempos will always be ratios with
  respect to prevaling tempo. Also ratios can be specified by omitting either
  number and the missing number is considered to be 1. Or a single int X means
  X:1. And a "vertical pipe character" divides the ratios at a sudden tempo
  change.

  Also update June 2016: changing ramp indicator to two dashes, not greater
  than/ less than symbols.

  Further change: now single = will be accepted as ramp indicator character

    (Legacy form supported for now.)

  In the following we call the prevailing tempo P

  3:2=    =1:2

    ramp from 3/2 P to 1/2 P. return to P afterward.

  1=   =2=    =4:5

    ramp from P to 2 P, then back to 4/5 P, and return to P

  1=   =1:2|4:5=  =1

    ramp from P to 1/2 P, then sudden change to 4:5 P, ramping back up to P

warps

  note: in warps with interval width specified, it can be an integer (not
  necessarily a ratio) -- however the amount of the warp has to be a ratio

  <1;:10 

     global warp because it has no S. Left-sided warp because < is on the
     left. warp interval of 1 beat because "1" is before the ";". Compresses
     interval because < is pointing in same direction as side it is on. Total
     compression amount is 1/10 of a beat.

  <+0.34<

     staff-only (because of the +), double-sided warp because there are '<' on
     each side. There is only one number inside the warp, so that number is
     the amount to compress left interval and stretch right interval, while
     look for w's to mark ends of interval

  <+1;:10<

     staff-only double-sided, left and right intervals are each 1 beat in
     duration

  <+1;%10

     staff-only single-sided. This will cause staves to get out of sync until
     the next one sided warp staff-only warp in the same warp that restores
     sync


  >%10> 

     global double-sided, stretch left interval and compress right interval by
     1/10 of a beat. look for w's to define intervals.

  3%2;1.6>

     global right-sided warp. The arrow pointing right means compress the
     interval. This doesn't actually affect the arrival time at the location
     the warp is marked but rather makes the next 3/2 of a beat speed up by a
     total amount of 1/8 a beat.
  

new kind of warp: to absolute final duration

  --> must use w marker

  --> called "absolute warp"

  <<1

    warp time so that duration between left w and this mark is 1 quarter

  1.2>>

    duration between this mark and right w should be 1.2 quarters

pause

  ps5 : NOT SUPPORTED ANY MORE

    legacy form: pause for 50 milliseconds

  ps@5

    new form: pause for 50 milliseconds

  ps%8

    pause for 1/8 of a quarter

  ps2%7

    pause for 2/7 of a quarter

  ps1.5

    pause for 1.5 quarters

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

    !wl!12

complex pauses

    multiple pause

      mp3/1/:6  --   mp<# pauses>/<beat separation of pauses>/<pause dur>



        insert pause of 1/6 quarters at current location, but also insert
        pauses of smaller duration in two previous locations (3 locations
        total), 1 beat before and 2 beats before. Pause lengths are scaled
        as arithmetic progression

    rit pause

      

      rp.95|1.1/1/:6    --   rp

        insert pause of :6 quarters. slow down for 1 beat prior to the
        pause to 95 percent of prevailing tempo. after the pause, tempo will
        be 110 percent of prevailing tempo and then slow down to prevailing
        tempo for 1 beat.

staff adjusts

  ^+:20 -- from left ^ to right ^ forms two sections. rit or accel left side
           in order to achive the specified delay (:20 of a beat). then
           adjust right side linearally to achieve overall same time

  ^^-:20 -- same as above, but global, and move arrival time earlier

  ^+:2;1 -- delay by half a quarter, with width of 1 quarter (basically
            assuming a ^ 1 beat to the left and a ^ one beat to the right)

rits then accel

   something equivalent to 1=   =0.95|1.05=   =1

   do it with              r         ra.95         r

   and have total time across span be unaffected

dynamic shapes

  <>.70/.50

    marks beginning of phrase that continues to <>+ mark. crescendo to an
    additional 50% dynamic step, then descrendo to original dynamic. peak of
    crescendo will occur 70% of way through shape

  <>

    marks end of <> shape

  ><.70/.50

    same as <> but descrendo follow by crescendo

  ><
 
    end of >< shape


bracketed areas

  [abc   abc]    : bracketed area of name "abc"


instrument technique

  some words are recognized as being instrument technique:

    arco, pizz., DXF

"two modify"

  tm[1=.9^][^/.9=1]n1

    two region modify, from t marker on left side to t marker on right side,
    with TempoModify's that ramp from 1 to 0.9, and on the other side, the
    multiplicative inverse of 0.9 to 1. left one is scaled parabolically in
    which fastest changing side is on the right. other one is scaled
    parabolically withi fastest changing side on the left. the final n1 means
    the final duration is normalized 90% of the way back toward original
    duration




-}

----------------------------------------------------------------------
----------------------------------------------------------------------


----------------------------------------------------------------------
----------------------------------------------------------------------
--              computeWordMarks

type MarkP = Mark NumVar


-- computeWordMarks
--
--   Returns map of all Marks with Loc and staff name as the keys,
--   and a map of variable values at each Loc where a variable is defined
--   or changes value
--
--   Follows these steps
--
--   First 
computeWordMarks:: Map String (Map Loc [String]) ->
                   Map Loc (Map String [MarkD])
computeWordMarks stavesWords = x2
  where
    pass1Words :: Map String (Map Loc [Pass1Word])
    pass1Words = M.map (lMapMap toPass1Word) stavesWords
    macroDefns :: Map String (Loc,MacroDefn)
    macroDefns = combineMacroDefns $ M.elems pass1Words
    x :: Map Loc (Map String [MarkP])
    x = flipMap $ M.map (pass1WordToMarkP macroDefns) pass1Words
    vars = computeVarMap x
    x2 = markPToMark vars x


mm :: Ord k => (a -> Maybe b) -> Map k a -> Maybe (Map k b)
mm f m | M.null m2 = Nothing
       | otherwise = Just m2
  where m2 = M.mapMaybe f m


-- Converts 
markPToMark :: Map Loc (Map String Double) -> Map Loc (Map String [MarkP]) ->
               Map Loc (Map String [MarkD])
markPToMark vars = M.mapMaybeWithKey g
  where
    g :: Loc -> Map String [MarkP] -> Maybe (Map String [MarkD])
    g loc = mm w
      where
        w :: [MarkP] -> Maybe [MarkD]
        w xs = case mapMaybe h xs of
          [] -> Nothing
          zs -> Just zs
        h :: MarkP -> Maybe MarkD
        h (SymbolMark s i) = Just $ SymbolMark s i
        h (SetTempo nv) = Just . SetTempo $ computeNumVar vars loc nv
        h SetVar{} = Nothing
        h (SpliceMark c) = Just $ SpliceMark c
        h (ArpDelta d) = Just $ ArpDelta d
        h (StacDur d) = Just $ StacDur d
        h (Trunc d)   = Just $ Trunc d
        h (Extend d)  = Just $ Extend d
        h (DynShape flag nv1 nv2) =
            Just $ DynShape flag (computeNumVar vars loc nv1)
                                 (computeNumVar vars loc nv2)
        h (Warp i1 i2 mNv1 nv2 flag) =
            Just $ Warp i1 i2 (fmap (computeNumVar vars loc) mNv1)
                   (computeNumVar vars loc nv2) flag
        h (AbsWarp s nv) = Just $ AbsWarp s (computeNumVar vars loc nv)
        h (Pause e) = case e of
            Left d   -> Just . Pause $ Left d
            Right nv -> Just . Pause . Right $ computeNumVar vars loc nv
        h (MultPause i1 nv1 nv2) =
            Just $ MultPause i1 (computeNumVar vars loc nv1)
                                (computeNumVar vars loc nv2)
        {-
        h (RitPause nv1 nv2 nv3 nv4) =
            Just $ RitPause (computeNumVar vars loc nv1)
                            (computeNumVar vars loc nv2)
                            (computeNumVar vars loc nv3)
                            (computeNumVar vars loc nv4)
        -}
        h (Adjust mNv1 nv2 dir flag) =
            let x = case mNv1 of
                  Just y -> Just $ computeNumVar vars loc y
                  Nothing -> Nothing
            in Just $ Adjust x (computeNumVar vars loc nv2) dir flag
        h AdjustMarker = Just AdjustMarker
        h W = Just W
        h (CrescDescr nv1 nv2) =
            Just $ CrescDescr (computeNumVar vars loc nv1)
                              (computeNumVar vars loc nv2)
        h EndCrescDescr = Just $ EndCrescDescr
        h EndDescrCresc = Just $ EndDescrCresc
        h (RampBeg nv)  = Just $ RampBeg $ computeNumVar vars loc nv
        h (RampEndBeg nv1 nv2) =
            Just $ RampEndBeg (computeNumVar vars loc nv1)
                              (computeNumVar vars loc nv2)
        h (RampEnd nv1) = Just $ RampEnd $ computeNumVar vars loc nv1
        h (TrillShapeMark x) = Just $ TrillShapeMark x
        h (TremShapeMark x) = Just $ TremShapeMark x
        h (PatternMark s) = Just $ PatternMark s
        h (Artic s)       = Just $ Artic s
        h (BracketL s)    = Just $ BracketL s
        h (BracketR s)    = Just $ BracketR s
        h RitAccel        = Just RitAccel
        h (TwoModify tm1 tm2 mNorm) = Just $ TwoModify tm1 tm2 mNorm


computeNumVar :: Variables -> Loc -> NumVar -> Double
computeNumVar vars loc (NumVar d mName) = case mName of
  Just s -> case M.lookupLE loc vars of
    Just (_,vs) -> case M.lookup s vs of
      Just x -> d*x
      Nothing -> t (printf "no variable '%s' defined" s)
    Nothing -> t (printf "no variables defined yet")
  Nothing -> d
  where
    t :: String -> Double
    t x = throwMine $ printf "at %s, %s" (showLoc2 loc) x


computeVarMap :: Map Loc (Map String [MarkP]) -> Map Loc (Map String Double)
computeVarMap mapIn = foldl step M.empty ys
  where
    xs :: [(Loc,[MarkP])]
    xs = M.toAscList $ M.map (concat . M.elems) mapIn
    ys :: [(Loc, (String,NumVar))]
    ys = mapMaybe g xs
    g :: (Loc,[MarkP]) -> Maybe (Loc,(String,NumVar))
    g (loc,marks) = fmap (loc,) . listToMaybe . mapMaybe maybeSetVar $ marks
    maybeSetVar :: MarkP -> Maybe (String,NumVar)
    maybeSetVar (SetVar s nv) = Just (s,nv)
    maybeSetVar _             = Nothing
    step :: Map Loc (Map String Double) -> (Loc, (String,NumVar)) ->
            Map Loc (Map String Double)
    step vars (locIn,(nameIn,(NumVar x mName))) = M.insert locIn newMap vars
      where
        errMsg l n = throwMine $ printf ("reference to variable at %s " ++
                     "named '%s'---it has not been defined") (showLoc2 l) n
        lastMap :: Map String Double
        lastMap = case M.maxView vars of
          Nothing    -> M.empty
          Just (m,_) -> m
        lookupLastValue n = case M.lookup n lastMap of
            Nothing -> errMsg locIn n
            Just d  -> d
        y = case mName of
          Nothing            -> 1
          Just nameRightSide -> lookupLastValue nameRightSide
        newMap :: Map String Double
        newMap = M.insert nameIn (y*x) lastMap
          
    
    

----------------------------------------------------------------------
----------------------------------------------------------------------
--        testing help


{-
-- we need to parse variables as part of this testing. where do we parse
-- variables? during translation of MarkWord to Mark
wordTesting :: Map Int TimeSig -> Map Loc [String] -> Map Loc [Mark]
wordTesting timeSigs words = markWordsToMarks timeSigs wLocs markWords
  where
    isW P1W = True
    isW _   = False
    pass1Words = lMapMap toPass1Word words
    macroDefns  = combineMacroDefns [pass1Words]    
    wLocs = S.fromList . map fst . filter (\(_,p) -> isW p) . lMapToList $
            pass1Words
    markWords = toMarkWords macroDefns pass1Words
-}


----------------------------------------------------------------------
----------------------------------------------------------------------



----------------------------------------------------------------------
----------------------------------------------------------------------
--                 pass 1 functions


-- Pass1Word includes macro definitions and instances that have not been
-- expanded yet. It includes any other string, that needs to be
-- parsed. Comments and also errors.


toPass1Word :: Loc -> String -> Pass1Word
toPass1Word loc s = case parse pass1Word "" s of
  Left err -> throwMine $ printf "XML Words parse error at %s:%s" 
              (showLoc2 loc) (show err)
  Right w  -> w


pass1Word = macroDefn <|> macroInstance <|> comment <|> 
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


macroInstance :: Parser Pass1Word
macroInstance = do
  char '!'
  name <- macroName
  char '!'
  fields <- sepBy1 (many1 $ noneOf "!") (char '!')
  eof
  return $ P1MacroInstance $ MacroInstance name fields


{-
-- must not consume any input if char 'w' matches, but not eof
w :: Parser Pass1Word
w = try $ do char 'w'
             eof
             return P1W
-}


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


pass1WordToMarkP :: Map String (Loc,MacroDefn) -> Map Loc [Pass1Word] ->
                    Map Loc [MarkP]
pass1WordToMarkP macros = lMapMaybeWithKey g
  where
    g :: Loc -> Pass1Word -> Maybe MarkP
    g loc (P1Normal s) = case parse mark "" s of
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

{-
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
-}



applyMacro :: Loc -> MacroDefn -> Loc -> [String] -> MarkP
applyMacro macroLoc (MacroDefn _ nArgs chars) instanceLoc args
  | nArgs /= length args = throwMine $ printf ("in macro instance at %s, "++
                           "number of args is %d. This does not match " ++
                           "definition at loc %s.") 
                           (showLoc2 instanceLoc) (length args) 
                           (showLoc2 macroLoc)
  | otherwise = case parse mark "" (concatMap g chars) of
                  Left err -> throwMine $ printf ("in parsing macro "++
                              "instance after subtitution: %s") (show err)
                  Right w  -> w
  where
    g (MdcChar c) = [c]
    g (MdcArg  i) = args !! (i-1)


----------------------------------------------------------------------
----------------------------------------------------------------------
--   Mark Parsers   Mark Parsers   Mark Parsers   Mark Parsers
----------------------------------------------------------------------
----------------------------------------------------------------------


mark :: Parser MarkP
mark =
  setTempo   <|> setVar      <|> splice     <|>
  arp        <|> stac        <|> warp       <|> absWarp    <|>
  pause      <|> multPause   <|> w          <|>
  rampBegin  <|> rampEnd     <|> rit        <|> accel      <|>
  trillShapeMark             <|> tremShapeMark             <|>
  patternMark  <|> dynShape  <|> endCrescDescr             <|> endDescrCresc
  <|> artic    <|> adjust    <|> trunc      <|> extend
  <|> bracketL <|> bracketR  <|> twoModify  <|> adjustMark



trunc :: Parser MarkP
trunc = do
  try $ string "trn"
  s <- seconds
  eof
  return $ Trunc s


extend :: Parser MarkP
extend = do
  try $ string "ext"
  s <- seconds
  eof
  return $ Extend s
  

----------------------------------------------------------------------
--                    arrival adjusts


adjustMark :: Parser MarkP
adjustMark = try $ char '^' >> eof >> return AdjustMarker


adjust :: Parser MarkP
adjust = try adjust_


adjust_ :: Parser MarkP
adjust_ = do
  char '^'
  glob <- option False (char '^' >> return True)
  (mWid,amt,dir) <- adjustContent
  return $ Adjust mWid amt dir glob


adjustContent = do
  dir <- (char '-' >> return (-1)) <|> (char '+' >> return 1)
  amt <- numVar
  mWid <- optionMaybe $ char ';' >> numVar
  eof
  return $ (mWid,amt,dir)


artic :: Parser MarkP
artic = do
  char '+'
  s <- many1 anyChar
  eof
  return $ Artic s


tempoModify :: Parser TempoModify
tempoModify = do
  char '['
  leftParab <- option False (char '^' >> return True)
  d1 <- tempoModNum
  char '='
  d2 <- tempoModNum
  rightParab <- option False (char '^' >> return True)
  char ']'
  case (leftParab,rightParab) of
    (False,False) -> return $ TmRamp d1 d2
    (True ,False) -> return $ TmRampParab d1 d2 True
    (False,True ) -> return $ TmRampParab d1 d2 False


tempoModNum =
  (do char '/'
      n <- num
      return $ 1/n)
  <|> num


twoModify :: Parser MarkP
twoModify = do
  try $ string "tm"
  tm1 <- tempoModify
  tm2 <- tempoModify
  norm <- optionMaybe (char 'n' >> num)
  eof
  return $ TwoModify tm1 tm2 norm

    
-- if T= succeeds, okay to assume this is setTempo. no variable starts with an
-- upper case letter
setTempo :: Parser MarkP
setTempo = do
  try $ string "T="
  n <- numVar
  eof
  return $ SetTempo n


-- try: variable name and =. then okay to assume this is setVar.
setVar :: Parser MarkP
setVar = do
  v <- try $ do s <- varName
                char '='
                return s
  n <- numVar
  eof
  return $ SetVar v n


-- no other mark starts with a $. Safe to consume it.
splice :: Parser MarkP
splice = do
  char '$'
  c <- oneOf ['a'..'z']
  eof
  return $ SpliceMark c


arp :: Parser MarkP
arp = do
  try $ string "arp"
  char '@'
  mult <- option 1 (char '-' >> return (-1))
  i <- parseInt
  eof
  return $ ArpDelta $ 0.01*mult*fromIntegral i


stac :: Parser MarkP
stac = do
  try $ string "stac"
  s <- seconds
  eof
  return $ StacDur s


-- does a try of something that succeeds consume input? yes
dynShape :: Parser MarkP
dynShape = do
  notFollowedBy $ try $ endCrescDescr <|> (endDescrCresc :: Parser MarkN)
  lr <- (do try $ string "<>"
            return True)
        <|>
        (do try $ string "><"
            return False)
  n1 <- numVar
  char '/'
  n2 <- numVar
  eof
  return $ DynShape lr n1 n2
  
  
warp = do
  -- This whole thing is in a 'try', so if the < matches the beginning of an
  -- absolute warp, the warpFollow will fail without consuming input.
  (try $ do notFollowedBy $ try $ string "<>"
            char '<'
            warpFollow (-1))
  <|>
  -- If we see > at beginning and it's not an endDescrCresc then this is a
  -- warp.
  (do notFollowedBy $ try $ string "><"
      char '>'
      warpFollow 1)
  <|>
  -- If the lookAhead succeeds then we can be sure it's a warp. It can't end
  -- with >> (that is, be a right-sided absolute warp) because the 'noneOf'
  -- won't match the first >.
  (do lookAhead $ try $ manyTill (noneOf "><") endsLikeWarp
      warpFollow 0)


warpFollow :: Int -> Parser MarkN
warpFollow leftDir = do
  glob        <- globalFlag
  (width,amt) <- warpInside
  rightDir    <- warpArrow
  eof
  return $ Warp leftDir rightDir width amt glob


endsLikeWarp :: Parser ()
endsLikeWarp = (char '>' <|> char '<') >> eof


globalFlag = option True (char '+' >> return False)


warpArrow :: Parser Int
warpArrow = (char '<' >> return (-1)) <|> (char '>' >> return 1) <|> return 0


warpInside :: Parser (Maybe NumVar,NumVar)
warpInside = do
  r1 <- numVar
  ((do char ';'
       r2 <- numVar
       return (Just r1,r2))
   <|>
   return (Nothing,r1))


-- beat value can be specified as single int, float, ratio of floats specified
-- with %, and any of that times a variable.
absWarp = 
  (do try $ string "<<"
      -- it's okay here to assume this is an absolute left sided warp
      v <- numVar
      eof
      return $ AbsWarp LeftWarp v)
  <|>
  (do lookAhead $ try $ manyTill (noneOf ">") endsLikeAbsWarp
      v <- numVar
      string ">>"
      eof
      return $ AbsWarp RightWarp v)


endsLikeAbsWarp = try (string ">>" >> eof)


pause = do
  try $ string "ps"
  notFollowedBy (char '@') <?> "the @ form of pause no longer accepted"
  v <- numVar
  eof
  return $ Pause (Right v)


multPause = do
  n <- try $ do string "mp"
                parseInt
  char '/'
  v1 <- numVar
  char '/'
  v2 <- numVar
  return $ MultPause n v1 v2

{-

ritPause = do
  n1 <- try $ do string "rp"
                 n <- numVar
                 char '|'
                 return n
  n2 <- numVar
  char '/'
  n3 <- numVar
  char '/'
  n4 <- numVar
  return $ RitPause n1 n2 n3 n4

-}


w = try (char 'w' >> eof >> return W)


rampBegin = do
  lookAhead $ try $ do many1 (notFollowedBy rampBegMarker >> anyChar)
                       rampBegMarker
                       eof
  r <- numVar
  rampBegMarker
  eof
  return $ RampBeg r


rampEnd = do
  try rampEndMarker
  r1 <- numVar
  mEndBeg <- maybeEndBeg
  case mEndBeg of
    Just (Just r2) -> return $ RampEndBeg r1 r2
    Just Nothing   -> return $ RampEndBeg r1 r1
    Nothing        -> return $ RampEnd r1


--
-- Returns Nothing if this is a ramp end (not an end/begin)
-- Returns (Just x) if this is end/begin, where x is:
--
--   - Nothing if there is only one ratio specified
--   - (Just y) if the second ratio specified is y
--
maybeEndBeg :: Parser (Maybe (Maybe NumVar))
maybeEndBeg = do
  -- This is case of an End/Beg that has split ratios
  (do char '|'
      r <- numVar
      rampBegMarker
      eof
      return $ Just (Just r))
  <|>
  -- This is case of an End/Beg with no second ratio
  (do rampBegMarker
      eof
      return $ Just Nothing)
  <|>
  -- This is the case of a simple ramp end.
  (do eof
      return Nothing)
     
  
rampBegMarker = (try $ string "--") <|> (try $ string "=")


rampEndMarker = (try $ string "--") <|> (try $ string "=")


rit = do
  try $ do string "rit."
           eof
  return RitAccel


accel = do
  try $ string "accel."
  return RitAccel


-- if we get 'tr' followed by ^ or v, okay to assume this is trillShapeMark
trillShapeMark = do
  step1 <- try $ do string "tr"
                    oneOf "^v"
  t <- trillShape step1
  eof
  return $ TrillShapeMark t


tremShapeMark = do
  step1 <- try $ do string "to"
                    choice [char '^', char 'v']
  t <- trillShape step1
  eof
  return $ TremShapeMark t


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

 
patternMark = do
  try $ string "pat:"
  PatternMark <$> many1 anyChar


endCrescDescr = try (string "<>" >> eof >> return EndCrescDescr)


endDescrCresc = try (string "><" >> eof >> return EndDescrCresc)



bracketL = do
  char '['
  ds <- many1 alphaNum
  eof
  return $ BracketL ds


bracketR = do
  lookAhead $ try $ many1 alphaNum >> char ']' >> eof
  ds <- many1 alphaNum
  char ']'
  eof
  return $ BracketR ds



----------------------------------------------------------------------
----------------------------------------------------------------------


-- what functions here might be done "just in time"? after all marks have been
-- parsed?
--
--   anything that refers to a variable, let's see, we'll have a variable map
--   available at the routine that processes, say, a warp
--
--   



{-

  this is stuff that has to do with converting MarkWord to Mark which I
believe has something to do with changing the form of warps and also computing
local values of variables



-- To do conversion from MarkWord to Mark. 
--
--   Input to this function
--  
--     (1) Map of Loc to PointType (this gives us endpoints for warps, <> and
--         ><
--     (2) Map of Loc to [MarkWord]
--
--   Computations:
--
--     (1) evaluate variables at each point.
--
--     (2) change the following MarkWord types to Mark
--
--           MwWarpTemp MwAbsWarpTemp, MwPause, MwMultPause, MwRitPause
--
markWordsToMarks :: Map Int TimeSig -> Set Loc -> Map Loc [MarkWord] ->
                    Map Loc [Mark]
markWordsToMarks timeSigs wLocs markWords = M.mapWithKey g markWords
  where
    varMap :: Map Loc (Map String Double)
    varMap = computeVariables markWords
    g :: Loc -> [MarkWord] -> [Mark]
    g loc = mapMaybe h
      where
        h :: MarkWord -> Maybe Mark
        h (MwMark m)             = Just m
        h t@MwWarpTemp{}         = Just $ toWarp    t varMap timeSigs loc wLocs
        h t@MwAbsWarpTemp{}      = Just $ toAbsWarp t varMap timeSigs loc wLocs
        h (MwSetTempo value)     = Just $ SetTempo 
                                   (evalNumVar loc value varMap)
        h MwSetVar{}             = Nothing
        h (MwPause (Left arg))   = Just $ Pause (Left arg)
        h (MwPause (Right arg))  = Just $ Pause 
                                   (Right $ evalNumVar loc arg varMap)


computeVariables :: Map Loc [MarkWord] -> Map Loc (Map String Double)
computeVariables markWords = foldl foldStep M.empty (M.toAscList markWords)
  where
    foldStep :: Map Loc (Map String Double) -> (Loc,[MarkWord]) -> 
                Map Loc (Map String Double)
    foldStep varMapIn (loc,words) = 
      case mapMaybe filterOneSetVar words of
        []                     -> varMapIn
        [(varName,beatsValue)] -> 
           M.insert loc (M.insert varName 
                                  (evalNumVar loc beatsValue varMapIn) 
                                  currentVars)
                    varMapIn
        _  -> throwMine $ printf "found more than one 'set variable' at %s"
              (showLoc2 loc)
      where
        currentVars :: Map String Double
        currentVars = case M.lookupLT loc varMapIn of
          Nothing    -> M.empty
          Just (_,v) -> v
        filterOneSetVar :: MarkWord -> Maybe (String,BeatsValue)
        filterOneSetVar (MwSetVar varName beatsValue) = 
          Just (varName,beatsValue)
        filterOneSetVar _ = Nothing


evalNumVar :: Loc -> BeatsValue -> Map Loc (Map String Double) -> Double
evalNumVar _   (BeatsValue x        Nothing) m = x
evalNumVar loc (BeatsValue x (Just varName)) m = 
  case M.lookupLE loc m of
    Nothing -> msg
    Just (_,latestVarMap) -> case M.lookup varName latestVarMap of
      Nothing -> msg
      Just y  -> x*y
    where
      msg = throwMine $ printf ("at %s, need to evaluate variable %s but it "++
            "is not defined at this point") (showLoc2 loc) varName


-- 
toWarp :: MarkWord -> Map Loc (Map String Double) -> Map Int TimeSig -> 
          Loc -> Set Loc -> Mark
toWarp (MwWarpTemp leftDir rightDir width amtIn globalFlag) varMap timeSigs 
       loc wMarks
  = Warp globalFlag leftLoc rightLoc amtOut (fromIntegral direction)
  where
    amtOut = evalNumVar loc amtIn varMap
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
        Just l  -> Just $ warpWarning timeSigs loc l
      Just w  -> 
        let wRational = approxRational (evalNumVar loc w varMap) 0.01
        in case locSub timeSigs loc wRational of
             Nothing -> throwMine msg4
             Just l  -> Just l
    computeRightLoc = case width of
      Nothing -> case S.lookupGT loc wMarks of
        Nothing -> throwMine $ msg3 "right"
        Just l  -> Just $ warpWarning timeSigs loc l
      Just w  ->
         let wRational = approxRational (evalNumVar loc w varMap) 0.01
         in case locAdd timeSigs loc wRational of
             Nothing -> throwMine msg5
             Just l  -> Just l


toAbsWarp :: MarkWord -> Map Loc (Map String Double) -> Map Int TimeSig -> 
            Loc -> Set Loc -> Mark
toAbsWarp (MwAbsWarpTemp side value) varMap timeSigs loc wLocs = 
  (AbsWarp theWLoc valueOut)
  where
    msg1 s = printf ("no %s W at warp at %s") s (showLoc2 loc)
    valueOut = evalNumVar loc value varMap
    theWLoc = case side of
      LeftWarp -> case S.lookupLT loc wLocs of
        Nothing -> throwMine $ msg1 "left"
        Just l  -> warpWarning timeSigs loc l
      RightWarp -> case S.lookupGT loc wLocs of
        Nothing -> throwMine $ msg1 "right"
        Just l  -> warpWarning timeSigs loc l




-- warpWarning
--
-- l1:: the loc at which the warp mark is located
-- l2:: the loc at which the adjacent w is located
--
-- In all cases, returns 'Just l2', but also gives a trace warning if
-- l2 is >=8 beats away from l1.
warpWarning timeSigs l1 l2
  | d >= 8 = printf "---WARNING--- w is pretty far away from warp at %s" 
             (simpleShowLoc l1) `trace` l2
  | otherwise = l2
  where
    d = if l1 < l2 
         then locDiff timeSigs l1 l2 
         else locDiff timeSigs l2 l1

-}


----------------------------------------------------------------------
----------------------------------------------------------------------
--  General parsers.

seconds = do 
  char '@'
  i <- parseInt
  return $ 0.01 * fromIntegral i



-- num examples
--
-- 3          integer: 3
-- 110        integer: 110
-- 3.1        float: 3.1
-- 100.3      float with 2 or more digits: 100.3 (not interpreted as percent)
-- 2:3        2/3
-- 10:12      10/12
-- 1.3:1.4    1.3/1.4  (can have ratio of floats)
-- :3         1/3
-- :12        1/12
num :: Parser Double
num =
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


-- numVar examples
--
-- a       variable alone
-- 3a      3 times a
-- 3       3
numVar = 
  (do n <- varName 
      return $ NumVar 1 (Just n))
  <|>
  (do n <- num
      v <- optionMaybe varName
      return $ NumVar n v)


{-
-- beats value example
--
-- 
beatsValue :: Parser BeatsValue
beatsValue = 
  (do v <- varName
      return $ BeatsValue 1 (Just v))
  <|>
  -- beatsValueNumericPart will not consume input if it fails
  (do n <- beatsValueNumericPart
      v <- optionMaybe varName
      return $ BeatsValue n v)


beatsValueOperator = char '%' <|> char ':'
-}


{-
-- beatsValueNumericPart will not consume input if it fails
beatsValueNumericPart :: Parser Double
beatsValueNumericPart = 
  -- four cases
  --    numeric, not followed by %
  --    % followed by numeric
  --    numeric followed by % and not followed by numeric
  --    numeric followed by % then numeric
  (try $ do d1 <- simpleDouble
            notFollowedBy beatsValueOperator
            return d1)
  <|>
  (try $ do beatsValueOperator
            d1 <- simpleDouble
            return $ 1/d1)
  <|>
  (try $ do d1 <- simpleDouble
            beatsValueOperator
            notFollowedBy simpleDouble
            return d1)
  <|>
  (try $ do d1 <- simpleDouble
            beatsValueOperator
            d2 <- simpleDouble
            return $ d1/d2)  
-}


simpleDouble :: Parser Double
simpleDouble = do
  s <- simpleDouble_dot <|> simpleDouble_notDot
  case reads s of
    (f,[]):_ -> return f
    _        -> fail "malformed numeric value"


simpleDouble_dot :: Parser String
simpleDouble_dot = do
  char '.'
  s <- many1 (oneOf "0123456789")
  return $ "0." ++ s


simpleDouble_notDot :: Parser String
simpleDouble_notDot = many1 $ oneOf ".0123456789"  


varName :: Parser String
varName = do
  c  <- lower
  cs <- many alphaNum
  return $ c:cs


{-
numRatio :: Parser NumRatio
numRatio =
  -- four cases
  --     (1) double alone
  --     (2) double plus :
  --     (3) : plus double
  --     (4) double : double
  (try $ do d1 <- simpleDouble
            notFollowedBy $ char ':'
            return $ NumRatio d1 1)
  <|>
  (try $ do n1 <- simpleDouble
            char ':'
            notFollowedBy $ try simpleDouble
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
-}


{-
numRatioPlusInt :: Parser NumRatio
numRatioPlusInt = 
  (try $ do x <- simpleDouble
            notFollowedBy (char ':')
            return $ NumRatio x 1)
  <|>
  numRatio
-}


{-
duration :: Parser Duration
duration = DurSecs <$> (try seconds) <|> DurBeats <$> (try beatsValue)
-}


{-

macros can be on different staves

  a new wrinkle with macros. we must have words from all staves in order to
  fully convert them to marks.


-}

----------------------------------------------------------------------

type MarkLoc = (Loc,[MarkD])

type MarkLocList = [MarkLoc]



matchBrackets :: Map Loc [MarkD] -> Map String [(Loc,Loc)]
matchBrackets marks = M.fromListWith (++) . concatMap f . L.tails
    $ M.toAscList marks
  where
    f :: [MarkLoc] -> [(String,[(Loc,Loc)])]
    f [] = []
    f ((loc,ms):remain) = map g $ mapMaybe maybeLeftBracket ms
      where
        g :: String -> (String,[(Loc,Loc)])
        g name = let loc2 = searchForRight loc name remain
                 in (name,[(loc,loc2)])


maybeLeftBracket :: MarkD -> Maybe String
maybeLeftBracket (BracketL s) = Just s
maybeLeftBracket _            = Nothing


searchForRight :: Loc -> String -> [MarkLoc] -> Loc
searchForRight loc name markLocList =
  case L.find (\(_,ms) -> isL name ms || isR name ms) markLocList of
    Nothing -> throwMine $ printf ("error: for BracketL mark at %s, " ++
      "with name '%s', no right bracket mark found") (showLoc2 loc) name
    Just (locR,ms)
      | isR name ms -> locR
      | isL name ms -> throwMine $ printf ("error: when " ++
          "searching for right bracket to match the left " ++
          "bracket mark at %s with name '%s', found another left bracket")
          (showLoc2 loc) name


isL :: String -> [MarkD] -> Bool
isL name marks = not $ null [s | BracketL s <- marks, s==name]

isR name marks = not $ null [s | BracketR s <- marks, s==name]


-- search for one that either isR or isL

----------------------------------------------------------------------
