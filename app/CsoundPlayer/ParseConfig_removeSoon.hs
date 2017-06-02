module App.CsoundPlayer.ParseConfig where

{-

                  EXAMPLE CONFIG STATEMENTS

-- comments: 
  [- <anything>]

-- mixer, wet/dry numbers; meaningful for csound only
[mixer 0.4 0.6]       

-- Indication of time divison
[timedivison 8]

-- When user enters a single measure number to start from, 
-- this is the number of blank measures that will terminate playback at that
-- point
[blanks 2]

-- map instrument
[mapinstr P1 instr100 0.5]  -- last number is pan

-- time warp :   (A COMMAND)
-- time warping with a simple 3-point warp curve that starts
--   at 1, goes to indicated peak, and back to 1. peaks of < 1 mean slow
--   down and peaks > 1 mean speed
-- 
--   [timewarp <start label> <end label> <value 1> <value 2> <value 3>
          <fraction of way through to reach value 3 peak as ratio> 
--   Note about timewarp: values 1/2/3 behave as follows: a higher value
--     means a faster tempo. Lower means slower. So regarding the modification
--     to the time map, the delta time intervals are actually divided by
--     the values, not multipled.
[timewarp P100 P200 1 1.5 1 2%3]

-- exaggerate time:  (A COMMAND)
--
-- pairs of beat durations B_d_1 through B_d_n, ratio R_1 through R_n
--  Beat duration refers to duration of a note as counted in "beats" which
--  means denominator of the measure (4 = quarter note is beat, 8=eigth note
--  as beat).
-- Beat durs are specified as Rationals.
--
-- (assuming beats durs are sorted in increasing order)
-- An entry (B_d_k R_k) means that every note with a duration D from
--   [B_d_k through B_d_(k+1) ) should have duration changed by ratio
--   R_k.
-- To be more exact, this means that any time slice in the time map
--  in which the shortest note
-- intersecting that time slice has duration D means that time slice should
-- be changed duration by ratio R_k.
--
-- There is an implicit (10000%1 1) added to this list. That is, if the
-- largest beat dur in this list is (B_largest R_largest), then every 
-- note with
-- beat duration from B_largest to 10000 will be treated with ratio
-- R_largest.
-- 
-- These entries do not have to be in order
--
[exaggeratetime (1%8 0.9) (1%4 0.92) (1%2 0.94) (1%1 0.96) (2%1 0.98) (4%1 1)]


-- ramp toward beat 1 (A COMMAND)
--    Adds a dynamic ramp in the measure
--    that gets louder toward beat 1 of the mext measure.
--    The given delta dynamics are interpretated as follows
--    <max delta dyn> : the dynamic delta added to notes that occur at beat 1
--    <min delta dyn> : the dynamic delta added to notes immediately following
--       beat 1
--
--    The delta dynamics: a value of 1.0 equals one dynamic level 
--    (e.g. p to mp or f to ff)
[ramptowardbeat1 <max delta dyn> <min delta dyn>]

-- measure time ramp
--    Put an accel or rit on each measure
--    Note: time slices in the tempo map are multiplied by the ratios, so
--    a number less than 1 speeds up, and > 1 slows down.
[measuretimeramp <time ratio, msr begin> <time ratio, msr end>]

-}

import qualified Data.Map as M
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Data.Ratio
import Data.Functor
import Util.Exception
import App.CsoundPlayer.CsoundPlayerData( ConfigStatement(..)
                                        , Command(..) 
                                        , AllConfig(..) ) 
import Csound.CsoundData( InstrMap(..)
                        , InstrPlay(..)
                        , Mixer(..) )

floatNum :: Parser Float
floatNum = do
  s <- many1 (oneOf "-0123456789.") 
  return $ read s

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

parseInstrPlay :: Parser (String,InstrPlay)
parseInstrPlay = do
  skipMany space
  partName <- many1 (alphaNum <|> char '-')
  skipMany space
  instrName <- many1 alphaNum
  skipMany space
  pan <- floatNum
  skipMany space
  trillRate <- floatNum
  skipMany space
  char ']'
  return (partName,InstrPlay instrName pan trillRate)

parseTimewarp :: Parser Command
parseTimewarp = do
  skipMany space
  loc1 <- many1 alphaNum
  skipMany space
  loc2 <- many1 alphaNum
  skipMany space
  value1 <- floatNum
  skipMany space
  value2 <- floatNum
  skipMany space
  value3 <- floatNum
  skipMany space
  frac <- rationalNum
  skipMany space
  char ']'
  return $ CommandTimewarp loc1 loc2 value1 value2 value3 frac

parseExTimeEntry :: Parser (Rational,Float)
parseExTimeEntry = do
  skipMany space
  char '('
  b <- rationalNum
  skipMany space
  r <- floatNum
  char ')'
  return (b,r)
  

parseExaggerateTime :: Parser Command
parseExaggerateTime = do
  etes <- many1 parseExTimeEntry
  skipMany space
  char ']'
  return $ CommandExaggerateTime (M.fromList etes)

parseRampTowardBeat1 :: Parser Command
parseRampTowardBeat1 = do
  skipMany space
  f1 <- floatNum
  skipMany space
  f2 <- floatNum
  skipMany space
  char ']'
  return $ CommandRampTowardBeat1 f1 f2

parseMeasureTimeRamp :: Parser Command
parseMeasureTimeRamp = do
  skipMany space
  f1 <- floatNum
  skipMany space
  f2 <- floatNum
  skipMany space
  char ']'
  return $ CommandMeasureTimeRamp f1 f2

parseTimeDivison :: Parser Int
parseTimeDivison = do
  skipMany space
  i <- intNum
  skipMany space
  char ']'
  return i

parseBlankMsrs :: Parser Int
parseBlankMsrs = do
  skipMany space
  i <- intNum
  skipMany space
  char ']'
  return i

parseMixer :: Parser Mixer
parseMixer = do
  skipMany space
  wet <- floatNum
  skipMany space
  dry <- floatNum
  skipMany space
  char ']'
  return $ Mixer wet dry

parseConfigStatement :: Parser ConfigStatement
parseConfigStatement = do
  char '['
  c <- (do try (string "exaggeratetime")
           ConfigCommand <$> parseExaggerateTime)
       <|>
       (do try (string "ramptowardbeat1")
           ConfigCommand <$> parseRampTowardBeat1)
       <|>
       (do try (string "measuretimeramp")
           ConfigCommand <$> parseMeasureTimeRamp)
       <|>
       (do try (string "timediv")
           ConfigTimeDivison <$> parseTimeDivison)
       <|>
       (do try (string "blankmsrs")
           ConfigBlankMsrs <$> parseBlankMsrs)
       <|>
       (do try (string "instr")
           (partId,ip) <- parseInstrPlay
           return $ ConfigInstrPlay partId ip)
       <|>
       (do try (string "mixer")
           ConfigMixer <$> parseMixer)
       <|>
       (do try (string "-")
           manyTill anyChar (char ']')
           return ConfigComment)
  skipMany space
  return c

parseAll :: Parser [ConfigStatement]
parseAll = do
  skipMany space
  cs <- many parseConfigStatement
  manyTill space eof
  return cs

-- Output
--  (<commands>, <map of all part names to instruments>, <mixer data>,
--    <number of time divisions of beat in time map> <number blank msrs to
--      treat as automatic stop point>)
parseConfigFile :: Parser ([Command],AllConfig)
parseConfigFile = do
   entries <- parseAll
   let coms = [c | (ConfigCommand c) <- entries]
       playDatas =   [(s,pd) | (ConfigInstrPlay s pd) <- entries]
       mixer = case mixers of 
         [m] -> m
         []   -> throwMine "no mixer statement in config"
         _    -> throwMine "multiple mixer statements in config"
         where
           mixers = [m | ConfigMixer m <- entries]
       timeDiv = case timeDivs of
         [d] -> d
         []  -> throwMine "no time divisions statement in config"
         _   -> throwMine "multiple time divisions statements in config"
         where
           timeDivs = [i | ConfigTimeDivison i <- entries]
       blankMsr = case blankMsrs of
         [d] -> d
         [] -> 1
         _   -> throwMine "multiple blankmsrs statements in config"           
         where
           blankMsrs = [i | ConfigBlankMsrs i <- entries]
   return ( coms
          , AllConfig (M.fromList playDatas) timeDiv blankMsr mixer
          )


{-
runParse :: String -> Either String Int
runParse s = case parse parseFile "" s of
  Left err -> Left $ show err
  Right x ->  Right x
-}


