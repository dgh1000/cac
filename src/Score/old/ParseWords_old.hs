{-



-}

module Score.ParseWords where


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

  t^25-93

    next trill will start at rate of 2, continue for 5 units accelerating
    toward rate of 9, then continue for 3 units. Total duration is 8 units. A
    unit in absolute terms will be 1/8 of the duration of the trill. Trill
    will start on upper note and end on lower note.

  t^25-93^

    same as above but trill will start AND end on upper note 

  tv25-95-00

    trill will start on lower note. rate 2, ramp for duration 5 units to 9,
    ramp for 5 units to 0, stay at 0 for 0 units. duration of unit is 1/10
    total length of trill. trill will end on upper note.

  trates;4;15

    set trill rate scale. The first number, 4, will be the rate of any trill
    marked as rate 0. The last number, 15 will be rate of a trill marked as
    rate 9. For rates from 1 to 8, scaled proportionally.


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
  case parse (parseWordMark timeSigs loc wMarks) "" s of
    Left e  -> throwMine $ printf "parse error in text at %s\n%s"
               (simpleShowLoc loc) (show e)
    Right x -> x


parseWordMark :: Map Int TimeSig -> Loc -> Set Loc -> Parser Mark
parseWordMark timeSigs loc wLocs =
 try parseInstrTechnique <|> try setTempo <|> try spliceMark <|> try arp <|>
 try stac <|> try pause <|> try (warp timeSigs loc wLocs) <|> try w <|> 
 try ritAccel <|> try ramp <|> try setVariable


w = char 'w' >> eof >> return WMark


ritAccel :: Parser Mark
ritAccel = (string "rit."   >> eof >> return RitAccel) 
           <|> 
           (string "accel." >> eof >> return RitAccel)



warp :: Map Int TimeSig -> Loc -> Set Loc -> Parser Mark
warp timeSigs loc wLocs =
  (do char '<'
      w <- warpInside timeSigs loc wLocs (-1) 
      eof
      return w)
  <|>
  (do w <- warpInside timeSigs loc wLocs 1
      char '>'
      eof
      return w)


warpInside :: Map Int TimeSig -> Loc -> Set Loc -> Double -> Parser Mark
warpInside timeSigs loc wLocs direction = do
  leftBar <- isJust `liftM` optionMaybe (char '|')
  (width,amount) <- warpValues
  rightBar <- isJust `liftM` optionMaybe (char '|')
  let (leftLoc,rightLoc,isGlobal) = case (leftBar,rightBar) of
        (False,False) -> case width of
          Nothing -> ( Just $ warpLeftLocW  loc wLocs
                     , Just $ warpRightLocW loc wLocs
                     , False )
          Just w  -> ( Just $ warpLeftLocWidth  timeSigs loc w
                     , Just $ warpRightLocWidth timeSigs loc w
                     , False )
        (l,r) -> case width of
          Nothing -> ( if l then Just $ warpLeftLocW  loc wLocs
                            else Nothing
                     , if r then Just $ warpRightLocW loc wLocs
                            else Nothing
                     , True )
          Just w ->  ( if l then Just $ warpLeftLocWidth  timeSigs loc w
                            else Nothing
                     , if r then Just $ warpRightLocWidth timeSigs loc w
                            else Nothing
                     , True )
  return $ Warp isGlobal leftLoc rightLoc direction amount


warpValues :: Parser (Maybe Rational,Rational)
warpValues = do
  value1 <- optionMaybe $ try rational 
                         <|> 
                         (do i <- parseInt
                             return $ fromIntegral i % 1)
  value2 <- optionMaybe (char ';' >> rational)
  case (value1,value2) of
    (Nothing, Nothing) -> fail ""
    (Just v1, Nothing) -> return (Nothing, v1)
    (Just v1, Just v2) -> return (Just v1, v2)


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


spliceMark = char '$' >> oneOf ['a'..'z'] >>= return . SpliceMark


parseInstrTechnique = do
  s <- choice $ map (try . string) ["DXF", "pizz.", "arco", "expr"]
  eof
  return $ InstrTechnique s 


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


duration :: Parser Duration
duration =
  (do char '@' 
      n <- simpleDouble
      return $ DurSecs (0.01*n))
  <|> 
  (DurBeats <$> rational)


beatDuration :: Parser Duration
beatDuration = do
  n1 <- option 1 parseInt
  char '%' <?> "expected a beat duration, requiring %"
  n2 <- parseInt
  return $ DurBeats (fromIntegral n1 % fromIntegral n2)


numRatio :: Parser NumRatio
numRatio = do
  n1 <- simpleDouble
  char ':'
  n2 <- simpleDouble
  return $ NumRatio n1 n2


 
simpleDouble :: Parser Double
simpleDouble = do
  s <- many1 (oneOf ".0123456789")
  case reads s of
    (f,[]):_ -> return f
    _        -> fail "malformed numeric value"


-- 12  @12
secondsDuration :: Parser Double
secondsDuration = optional (char '@') >> parseInt >>= 
  return . (*0.01) . fromIntegral


varName :: Parser Char
varName = oneOf ['a'..'z']


-- For absolute tempos, can only handle quarters per minute right now 
--
-- 100 Q
-- a
-- 3:2a
-- 3:2
tempo :: Parser Tempo
tempo =
  try absoluteTempo <|> relativeTempo


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


-- like 'tempo' but a tempo relative to prevailing tempo is not allowed
rampEndTempo :: Parser (Either NumRatio Tempo)
rampEndTempo =
  (try $ do t <- absoluteTempo
            return $ Right t)
  <|>
  (try $ do r <- option (NumRatio 1 1) numRatio
            v <- varName
            return $ Right $ TempoRelative r (Just [v]))
  <|>
  (do r <- numRatio
      return $ Left r)
   


-- set tempo is "T=" followed by tempo expression
setTempo :: Parser Mark
setTempo = do
  string "T="
  many space 
  t <- tempo
  eof
  return $ SetTempo t


-- set variable is variable name followed by = followed by tempo
setVariable :: Parser Mark
setVariable = do
  n <- varName
  many space
  char '='
  many space
  t <- tempo
  eof
  return $ SetVar [n] t


-- >>   <<   <<|  3:2>>  100Q>>  a>>  <<5:4|  <<3:2b 
--
ramp :: Parser Mark
ramp = 
  (do r <- option (NumRatio 1 1) numRatio
      string ">>"
      eof
      return $ RampBeg r)
  <|>
  (try $ do string "<<"
            t <- option (Left $ NumRatio 1 1) rampEndTempo
            stop <- isJust `liftM` optionMaybe (char '|')
            eof
            return $ RampEnd t stop)
  <|>
  (do string "<<"
      t <- option (NumRatio 1 1) numRatio
      string ">>"
      eof
      return $ RampEndBeg t)    




arp :: Parser Mark
arp = do
  string "arp"
  d <- secondsDuration
  eof
  return $ ArpDelta d


stac :: Parser Mark
stac = do
  string "stac"
  d <- secondsDuration
  eof
  return $ StacDur d


pause :: Parser Mark
pause = do
  string "ps"
  (try (do i <- parseInt
           eof
           return $ Pause $ DurSecs (0.01 * fromIntegral i))
   <|>
   (do d <- duration
       eof
       return $ Pause d))

  


----------------------------------------------------------------------
----------------------------------------------------------------------
--                util


             
     
      
----------------------------------------------------------------------
----------------------------------------------------------------------
-- debug

      





