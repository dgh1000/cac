{-# LANGUAGE FlexibleContexts #-}


module Score.ParseWords where


import qualified Data.Map as M
import qualified Data.Set as S
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
import Common.CommonUtil(locAdd,locSub,locDiff, simpleShowLoc)
import Common.CommonData
import Util.Exception
import Util.Math(scale)
import XmlDoc.XmlDocData



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

  in the following, call the prevailing tempo P. Note that references to
  ratios of the prevailing tempo are done without a *

  3:2>>    <<1:2|

    ramp from 3/2 the prevailing tempo P to 1/2 P. return to P afterward

  >>    <<1:2

    ramp from prevailing tempo P to  1/2 P and continue at 1/2P

  1:2>>   <<3:2a

    ramp from 1/2 P to 3/2 a (a is a variable) and continue at 3/2 a

  >>     <<100Q 

    ramp from P to 100 bpm. continue at 100Q.

  >>   <<2:1>>    <<4:5|

    ramp from P to 2/1 P, then back to 4/5 P, and continuing at P

  >>   <<>>   <<

    this is legal. wherever tempo is missing, it means 1/1 P


warps

  note: in warps with interval width specified, it can be an integer (not
  necessarily a ratio) -- however the amount of the warp has to be a ratio

  <|1;%10 

     global warp because it has a | (staff warps have no |). Left-sided warp
     because | is on the left. warp interval of 1 beat because "1" is before
     the ";". Compresses interval because < is pointing left. Total
     compression amount is 1/10 of a beat.

  <%10    

     staff-only double-sided warp (because there are no | that makes it both
     staff-only and double-sided). There is only one number inside the warp,
     so that number is the amount to compress left interval and stretch right
     interval, while look for w's to mark ends of interval

  <1;%10

     staff-only double-sided, left and right intervals are each 1 beat in
     duration

  |%10|> 

     global double-sided, stretch left interval and compress right interval by
     1/10 of a beat. look for w's to define intervals.

  3%2;%8|>

     global right-sided warp. The arrow pointing right means that right
     interval will be compressed. This doesn't actually affect the arrival
     time at the location the warp is marked but rather makes the next 3/2 of
     a beat speed up by a total amount of 1/8 a beat.


pause

  ps5

    legacy form: pause for 50 milliseconds

  ps@5

    new form: pause for 50 milliseconds

  ps%8

    pause for 1/8 of a beat

  ps2%7

    pause for 2/7 of a beat


arpeggio and staccato durations

  arp10 

    legacy form: set arp delta to 10*10 milliseconds

  stac5

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


trill

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


-}






computeWordMarks :: Map Int TimeSig -> Map Loc [XMsrData] -> Map Loc [Mark]
computeWordMarks timeSigs msrData = 
    M.mapWithKey (\k ss -> map (toWordMark timeSigs k wLocs) ss) words
  where 
    wLocs = S.fromList . mapMaybe maybeWMark . lMapToList $ words
    words  :: Map Loc [String]
    words  = lMapMaybe maybeWords msrData


maybeWMark :: (Loc,String) -> Maybe Loc
maybeWMark (loc,ss) | isWordMark ss = Just loc
                    | otherwise     = Nothing
  where
    isWordMark s = case parse w "" s of
      Left _  -> False
      Right _ -> True


maybeWords :: XMsrData -> Maybe String
maybeWords (XMDDirection (XDWords s _) _ _ _) = Just s
maybeWords _ = Nothing


toWordMark :: Map Int TimeSig -> Loc -> Set Loc -> String -> Mark
toWordMark timeSigs loc wMarks s =
  case parse mark "" s of
    Left e -> throwMine $ printf "parse error in text at %s\n%s"
              (simpleShowLoc loc) (show e)
    Right m -> case m of
      w@ WarpTemp {} -> toWarp w timeSigs loc wMarks
      m              -> m


toWarp :: Mark -> Map Int TimeSig -> Loc -> Set Loc -> Mark
toWarp (WarpTemp leftBar rightBar mWidth amt direc) timeSigs loc wMarks =
    Warp glob leftLoc rightLoc amt direc
  where
    glob = leftBar || rightBar
    phrase = if glob then "global" else "staff"
    -- There is a left loc under two conditions:
    --    This is a staff warp. In this case 'leftBar' and 'rightBar' will
    --       be False. 
    --    This is a global warp (at least one of 'leftBar' and 'rightBar' is 
    --       true) and 'leftBar' is True.
    leftLoc | not leftBar && rightBar = Nothing
            | otherwise = case mWidth of
      Nothing -> case S.lookupLT loc wMarks of
        Nothing -> throwMine $ printf "no left w in %s warp at %s" phrase
                   (simpleShowLoc loc)
        Just l  -> warpWarning timeSigs loc l
      Just w -> case locSub timeSigs loc w of
        Nothing -> throwMine $ printf ("left interval prior to composition "++
                   "start: at warp at %s") (simpleShowLoc loc)
        Just l  -> Just l
    -- There is a right loc under two conditions:
    --    This is a staff warp. In this case 'leftBar' and 'rightBar' will
    --       be False. 
    --    This is a global warp (at least one of 'leftBar' and 'rightBar' is 
    --       true) and 'rightBar' is True.
    rightLoc | not rightBar && leftBar = Nothing
             | otherwise = case mWidth of
      Nothing -> case S.lookupGT loc wMarks of
        Nothing -> throwMine $ printf "no right w in %s warp at %s" phrase
                   (simpleShowLoc loc)
        Just l  -> warpWarning timeSigs loc l
      Just w -> case locAdd timeSigs loc w of
        Nothing -> throwMine $ printf ("right interval after composition "++
                   "end: at warp at %s") (simpleShowLoc loc)
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


w = char 'w' >> eof >> return WMark


mark =
  try (char 'w' >> eof >> return WMark)
  <|>
  (try (string "T=") >> setTempo)

setTempo = do
  

mark :: Parser Mark
mark =
  -- w
  (do try (char 'w') >> eof
      return WMark)
  <|>
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
  -- trill shape
  (do step1 <- try $ do string "tr"
                        choice [char '^', char 'v']
      t <- trillShape step1
      return $ TrillShapeMark t)
  <|>
  -- tremolo shape
  (do step1 <- try $ do string "to"
                        choice [char '^', char 'v']
      t <- trillShape step1
      return $ TremShapeMark t)
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
  (do try $ string "<<"
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
      ((do r <- try rational
           eof
           return $ Pause $ DurBeats r)
       <|>
       (do char '@'
           s <- seconds
           eof
           return $ Pause $ DurSecs s)
       <|>
       (do s <- seconds
           eof
           return $ Pause $ DurSecs s)))
  <|>
  (do try $ string "arp"
      many $ char '@'
      mult <- option 1 (char '-' >> return (-1))
      ds <- many1 digit
      eof
      return $ ArpDelta $ 0.01*mult*(read ds))
  <|>
  (do try $ string "stac"
      s <- seconds
      eof
      return $ StacDur s)
  <|>
  (try (string "rit.") >> return RitAccel)
  <|>
  (try (string "accel.") >> return RitAccel)
      
 

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
  ((try $ do f1 <- simpleDouble
             char ':'
             f2 <- simpleDouble
             ((do char '*'
                  return $ TempoRelative (NumRatio f1 f2) Nothing)
              <|>
              (do v <- varName
                  return $ TempoRelative (NumRatio f1 f2) (Just [v]))))
   <|>
   (do ds <- many1 digit
       char 'Q'
       return $ TempoAbs (read ds)))


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

