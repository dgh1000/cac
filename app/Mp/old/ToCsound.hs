
module App.Mp.ToCsound where

import Debug.Trace
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Debug.Trace
import Text.Printf
import Util.Math
import Util.FileUtil
import System.Process
import Midi.MidiData
import Util.Exception

----------------------------------------------------------------------
----------------------------------------------------------------------
-- top level function in this module

runCsound :: [MidiEvent] -> IO ()
runCsound evts = do
  let configFilename = "/haskell/Util/softsynth/config.txt"
  dvs <- readConfigTxt
  let methodFn = case lkInt "method" dvs of
        8 -> "======= 8: single waveform ===============" `trace` method8
        9 -> "===== 9: individual partials==============" `trace` method9
        _ -> throwMine "unknown method specified in config file"
  writeCsd (concatMap scoreStmToString $ methodFn dvs evts)
  system $ printf "csound -d -W -m 0 -o \\Users\\Mike\\out.wav csd.csd" 
  -- system "csound -odac7 -d \\haskell\\Util\\softsynth\\playback.csd"
  return ()



----------------------------------------------------------------------
--                      data

data DataValue = DVInt    { cvName :: String, cvInt    :: Int    }
               | DVDouble { cvName :: String, cvDoub   :: Double }
               | DVBool   { cvName :: String, cvBool   :: Bool   }
               | DVCurve  { cvName :: String, cvPoints :: [(Double,Double)] }


data DataValueType = TInt | TDouble | TBool | TCurve


data ScoreStm = IStm Int Double Double [DataValue]
              | FStm String


dataValueToString :: DataValue -> String
dataValueToString (DVInt    _ i) = printf "%20d"   i
dataValueToString (DVDouble _ d) = printf "%20.5f" d
dataValueToString (DVBool   _ b) = printf "%20s"   (show b)
dataValueToString (DVCurve  _ _) = "n/a: curve"


scoreStmToString :: ScoreStm -> String
scoreStmToString (IStm iNum tBeg dur vs) =
  printf "i%19s%20.5f%20.5f%s\n" (show iNum) tBeg dur 
         (concatMap dataValueToString vs)
scoreStmToString (FStm s) = s ++ "\n"


----------------------------------------------------------------------

int :: Parser Int
int = do
  ds <- many1 digit
  case reads ds of
    (i,_):_ -> return i
    _       -> fail "bad integer"

double :: Parser Double
double = do
  ds <- many1 (oneOf "0123456789.-")
  case reads ds of
    (d,_):_ -> return d
    _       -> fail "bad double"

bool :: Parser Bool
bool = (string "True" >> return True) <|> (string "False" >> return False)


comment = do
  char '{'
  manyTill anyChar (char '}')
  many space
  return Nothing


curve :: Parser [(Double,Double)]
curve = do
  let pair :: Parser (Double,Double)
      pair = do
        char '('
        many space
        x <- double
        many space
        char ','
        many space
        y <- double
        many space
        char ')'
        many space
        return (x,y)
  char '['
  many space
  ps <- many1 pair
  char ']'
  many space
  return ps


configValue :: Parser (Maybe DataValue)
configValue = do
  name <- many1 (alphaNum <|> char '_')
  many space
  typ  <- ((char 'i' >> return TInt   )
           <|>
           (char 'b' >> return TBool  )
           <|>
           (char 'd' >> return TDouble)
           <|>
           (char 'c' >> return TCurve ))
  many space
  v <- case typ of
    TInt    -> do { x <-    int; return $ Just $ DVInt    name x }
    TDouble -> do { x <- double; return $ Just $ DVDouble name x }
    TBool   -> do { x <- bool  ; return $ Just $ DVBool   name x }
    TCurve  -> do { x <- curve ; return $ Just $ DVCurve  name x }
  many space
  return v



{-
configItem :: Parser (Maybe DataValue)
configItem = do
  skipMany space
  v <- (comment <|> configValue)
  skipMany space
  return v
-}

parseDataValues :: Parser [DataValue]
parseDataValues = do
  skipMany space
  vs <- many (comment <|> configValue)
  return $ catMaybes vs


readConfigTxt :: IO [DataValue]
readConfigTxt = do
  b <- readFile "/haskell/Util/softsynth/config.txt"
  case parse parseDataValues "" b of
    Left  err -> throwMine $ "error parsing config.txt: " ++ (show err)
    Right vs  -> return vs


lkDoub :: String -> [DataValue] -> Double
lkDoub name vs = case [d | DVDouble s d <- vs, s == name] of
  []  -> throwMine $ printf "in lkDouble, can't find field '%s'" name
  [x] -> x
  _   -> throwMine $ printf "in lkDouble, multiple fields '%s'"  name


lkInt :: String -> [DataValue] -> Int
lkInt name vs = case [i | DVInt s i <- vs, s == name] of
  []  -> throwMine $ printf "in lkInt, can't find field '%s'" name
  [x] -> x
  _   -> throwMine $ printf "in lkInt, multiple fields '%s'"  name


lkBool :: String -> [DataValue] -> Bool
lkBool name vs = case [d | DVBool s d <- vs, s == name] of
  []  -> throwMine $ printf "in lkDouble, can't find field '%s'" name
  [x] -> x
  _   -> throwMine $ printf "in lkDouble, multiple fields '%s'"  name



lkCurve :: String -> [DataValue] -> [(Double,Double)]
lkCurve name vs = case [c | DVCurve s c <- vs, s == name] of
  []  -> throwMine $ printf "in lkDouble, can't find field '%s'" name
  [x] -> x
  _   -> throwMine $ printf "in lkDouble, multiple fields '%s'"  name


----------------------------------------------------------------------
-- utility

midiPitchToCps :: Int -> Double
midiPitchToCps p = 440 * 2 ** ((fromIntegral p-69)/12)


-- never does stretch tuning
midiFreq2 :: Int -> Int -> Double
midiFreq2 p harm = fromIntegral harm * 440 * 2 **((fromIntegral p-69)/12)


midiFreq1 :: [DataValue] -> Int -> Int -> Double
midiFreq1 dvs pitch harmNum =
    baseFreq * fromIntegral harmNum ** c
  where
    b = lkDoub "tuningBase" dvs
    baseFreq = 440 * b ** ((fromIntegral pitch-69)/12)
    c = log b / log 2


midiPitchToCps2 :: Double -> Int -> Double
midiPitchToCps2 base p = 440 * base ** ((fromIntegral p-69)/12)




----------------------------------------------------------------------
--              utility

-- returns (tBeg,tEnd,amp,pitch)
evtData :: MidiEvent -> (Double,Double,Double,Int)
evtData (NoteEvent t tOff evtOn evtOff _ _ _ _ _ _) = (tBeg,tEnd,amp,pitch)
  where
    tBeg  = fromIntegral t    / 1000
    tEnd  = fromIntegral tOff / 1000
    pitch = rmeData1 evtOn
    vel   = rmeData2 evtOn
    amp   = 100 * ( exp $
                  scale (0::Double) (fromIntegral vel) 127 (log 1) (log 60)
                  )

getIStmEnd :: ScoreStm -> Maybe Double
getIStmEnd (IStm _ t dur _) = Just $ t+dur
getIStmEnd _                = Nothing


-- mixerI
--
-- Takes list [(<event end time>, <i statement(s) for event>)]
--
mixerI :: [(Double,String)] -> String
mixerI xs = printf "i1000 0 %f\n" ((maximum $ map fst xs) + 1.5)


normalizeIStms :: [ScoreStm] -> [ScoreStm]
normalizeIStms ss = map g ss
  where
    b (IStm _ t _ _) = Just t
    b (FStm _)       = Nothing
    minB = case mapMaybe b ss of
      [] -> throwMine "in ToCsound.hs, no i statements to normalize"
      xs -> minimum xs
    g (IStm iNum beg dur vs) = IStm iNum (beg-minB+0.1) dur vs
    g (FStm s)               = FStm s


interpolateCurve :: Double -> [(Double,Double)] -> Double
interpolateCurve x pairs
  | length pairs < 2 = throwMine "in interpolateCurve, less than 2 pairs"
  | otherwise        = case mapMaybe g pairedPairs of
                         (y:_) -> y
  where
    pairedPairs = zip pairs (drop 1 pairs)
    g ((x1,y1),(x2,y2)) 
      | x1 <= x && x <= x2 = Just $ scale x1 x x2 y1 y2
      | otherwise          = Nothing


-- interpolateEqCurve
--
--   Given an EQ curve, interpolate it and return result expressed as ratio
--   to zero gain. Interpolates frequency logarithically.
--
-- Double            :: frequency in
-- [(Double,Double)] :: [(<x points in Hz>, <y points in dB>)]
-- 
-- Output: ratio to zero gain (so 1 would be zero gain, -0.5 would be -6 dB,
--         etc)
interpolateEqCurve :: Double -> [(Double,Double)] -> Double
interpolateEqCurve x pairs
  | length pairs < 2 = throwMine "in interpolateCurve, less than 2 pairs"
  | otherwise        = case mapMaybe g pairedPairs of
                         (y:_) -> y
  where
    dbToRatio d = 10 ** (d/20)
    pairedPairs = zip pairs (drop 1 pairs)
    g ((x1,y1),(x2,y2)) 
      | x1 <= x && x <= x2 = Just $ dbToRatio $ 
                             scale (log x1) (log x) (log x2) y1 y2
      | otherwise          = Nothing


computeEq dvs freq = interpolateEqCurve freq (lkCurve "eqCurve" dvs)



-- modified sawtooth
--
-- 1/partialNum**exp partial strengths with possible rotated phase
--
-- 'exp' is field ccModSawtoothExp
--
-- Output is [(<partial num>, <strength>, <phase>)]
mkModifiedSawtooth :: [DataValue] -> Int -> [(Int,Double)] -> 
                      [(Int,Double,Double)]
mkModifiedSawtooth cvs nPartials freqs = map g freqs
  where
    g :: (Int,Double) -> (Int,Double,Double)
    g (n,freq) = (n,strength2,phase)
      where
        rotatePhase = lkBool "rotatePhase" cvs
        phase 
          | rotatePhase   = fromIntegral (n-1) / fromIntegral nPartials
          | otherwise          = 0
        applyEqFlag    = lkBool "applyEq" cvs
        strength1   = 1/fromIntegral n ** (lkDoub "modSawtoothExp" cvs)
        strength2 | applyEqFlag = strength1 * computeEq cvs freq
                  | otherwise   = strength1

            
           

-- String :: score section of csd
writeCsd :: String -> IO ()
writeCsd sco = do
  b <- readFile "/haskell/Util/softsynth/templates/exper-template.csd"
  writeFileStrictly "csd.csd" $ printf b sco
  

{-
makeI :: String -> [DataValue] -> String
makeI prefix vs = prefix ++ " " ++ concatMap toString vs ++ "\n"
  where
    toString :: DataValue -> String
    toString (DVInt _ i) = printf " %d" i
    toString (DVDouble _ d) = printf "%12.5f" d
-}

----------------------------------------------------------------------
-- 

-- top level method1. take all midi events and translate each one to I
-- statements of type i5. then write tables.txt and sco.sco
--
-- never does stretch tuning
--


-- make an 09 table as needed by method8. (Table number is the midi pitch it
-- be used for.)
mkGen09_method8 :: [DataValue] -> Int -> [(Int,Double,Double)] -> ScoreStm
mkGen09_method8 cvs pitch partials = 
    FStm $ printf "f%d 0 %d -9 %s\n" pitch (lkInt "gen9TableSize" cvs)
                  (concatMap g partials)
  where
    g :: (Int,Double,Double) -> String
    g (n,strength,phase) = printf "   %d %.5f %.1f" n strength (360*phase)


method8 :: [DataValue] -> [MidiEvent] -> [ScoreStm]
method8 dvs evts = IStm 1000 0 (maxEnd+1.0) [] : (all ++ tables)
  where
    -- CONFIGURATION
    mkPartials      = mkModifiedSawtooth dvs -- 1/n method with possible
                                             -- omitted partials, flag to check
                                             -- whether to rotate phase
    mkMidiFreq      = midiFreq2              -- no stretching of harmonics
    pitches         = [24,27..108]
    pitchDelta      = 3

    -- make tables
    tables = map makeOneTable pitches
    makeOneTable :: Int -> ScoreStm
    makeOneTable p = mkGen09_method8 dvs p partials
      where
        -- need to determine how many harmonics. That requires fundamental 
        -- frequency and max freq.
        fund      = mkMidiFreq p 1
        nPartials = floor $ lkDoub "maxFreq" dvs / fund
        pInput    = map (\n -> (n,mkMidiFreq p n)) [1..nPartials]
        partials  :: [(Int,Double,Double)]
        partials  = mkPartials nPartials pInput
    
    -- translate events
    -- convert all events and concat ScoreStms. add mixer statement.
    all     = normalizeIStms $ mapMaybe evt2I evts
    maxEnd  = case mapMaybe getIStmEnd all of
      [] -> throwMine "error in method9"
      xs -> maximum xs

    evt2I SingleEvent{}   = Nothing
    evt2I evt@NoteEvent{} = Just out
      where
        (tBeg,tEndN,ampl,pitch) = evtData evt
        release = lkDoub "release_8" dvs
        tEnd = tEndN+release
        dur = tEnd-tBeg
        freq = mkMidiFreq pitch 1
        out = IStm 8 tBeg dur [ DVDouble "" ampl
                              , DVDouble "" freq
                              , DVInt    "" pitch
                              , DVDouble "" (lkDoub "att_8"         dvs)
                              , DVDouble "" (lkDoub "decayDbSec_8"  dvs)
                              , DVDouble "" (lkDoub "lpCo1_8"       dvs)
                              , DVDouble "" (lkDoub "lpCo2_8"       dvs)
                              , DVDouble "" (lkDoub "lpPhase1Dur_8" dvs)
                              , DVDouble "" (lkDoub "lpDecOctSec_8" dvs)
                              , DVDouble "" release
                              , DVDouble "" (lkDoub "lpEnd_8"       dvs)
                              , DVDouble "" pitchDelta
                              ]


----------------------------------------------------------------------
----------------------------------------------------------------------
--    method multiple sine: instr 9

{-


  can we use existing code in computing partials?

    I think so: mkModifiedSawtooth returns partial number and strength

  how do we compute frequency of partials?

    midiFreq1 takes configuation, midi pitch number, and harmonic number

  how do we select envelopes (attack, decay rate, release)? 
 
    probably have a min and a max: scale over frequency spectrum,
    logarithmically: over what frequency range should we scale? use scaleClip
    I think

  how do we select jitter amounts? varies over range of harmonics I
  think. again logarithmic with actual frequency



-}


method9 :: [DataValue] -> [MidiEvent] -> [ScoreStm]
method9 dvs evts = IStm 1000 0 (maxEnd+1) [] : all
  where
    -- configuration
    mkPartials   = mkModifiedSawtooth dvs -- 1/n method with possible
                                            -- omitted partials, flag to check
                                            -- whether to rotate phase
    mkMidiFreq      = midiFreq1 dvs   -- takes midi pitch and harmonic
                                        -- number.  can do streth tuning in
                                        -- the sense of changing the
                                        -- fundamental frequency, but all
                                        -- harmonics are multiples of
                                        -- fundamental

    -- convert all events and concat ScoreStms. add mixer statement.
    all     = normalizeIStms $ concat $ mapMaybe evt2I evts
    maxEnd = case mapMaybe getIStmEnd all of
      [] -> throwMine "error in method9"
      xs -> maximum xs

    -- function to convert one event
    evt2I :: MidiEvent -> Maybe [ScoreStm]
    evt2I SingleEvent{}   = Nothing
    evt2I evt@NoteEvent{} = Just iStms
      where
        (tBeg,tEndN,ampl,pitch) = evtData evt
        fund      = mkMidiFreq pitch 1
        nPartials = floor $ lkDoub "maxFreq" dvs / fund
        -- must have at least 2 partials to avoid NaN results in
        -- jitter computations
        pInput    = map (\n -> (n,mkMidiFreq pitch n)) [1..nPartials]
        partials  :: [(Int,Double,Double)]
        partials  = mkPartials nPartials pInput
        maxPartial = maximum $ map (\(x,_,_) -> x) partials
        iStms = map doOnePartial partials

        -- function called for each partial
        doOnePartial :: (Int,Double,Double) -> ScoreStm
        doOnePartial (partialNum,strength,_) = out
          where
            -- read configuration
            attMin      = lkDoub "att_9_min"     dvs
            attMax      = lkDoub "att_9_max"     dvs
            relMin      = lkDoub "rel_9_min"     dvs
            relMax      = lkDoub "rel_9_max"     dvs
            decayMin    = lkDoub "decay_9_min"   dvs
            decayMax    = lkDoub "decay_9_max"   dvs
            freqJit     = lkDoub "freqJit_9"     dvs
            amplJitMin  = lkDoub "amplJit_9_min" dvs
            amplJitMax  = lkDoub "amplJit_9_max" dvs
            jitCpsMin   = lkDoub "jitCps_9_min"  dvs
            jitCpsMax   = lkDoub "jitCps_9_max"  dvs
            testHpCo    = lkDoub "test_hp_co"    dvs

            -- scale factors
            freq = mkMidiFreq pitch partialNum
            logFreq     = log freq
            logMin      = log $ lkDoub "scalingFreqMin" dvs
            logMax      = log $ lkDoub "scalingFreqMax" dvs

            -- compute envelopes
            att         = scaleClip logMin logFreq logMax attMin attMax
            rel         = scaleClip logMin logFreq logMax relMin relMax
            decay       = scaleClip logMin logFreq logMax decayMin decayMax

            -- jitter
            l2          = log $ fromIntegral partialNum
            l3          = log $ fromIntegral maxPartial
            -- freqJit     = scaleClip 0 l2 l3 freqJitMin freqJitMax
            amplJit     = scaleClip 0 l2 l3 amplJitMin amplJitMax

            -- compute timing
            tEnd        = tEndN + rel
            dur         = tEnd - tBeg 

            -- make i statement              
            out = IStm 9 tBeg dur
                  [ DVDouble "" (ampl*strength)
                  , DVDouble "" freq
                  , DVInt    "" pitch
                  , DVDouble "" att
                  , DVDouble "" decay
                  , DVDouble "" rel
                  , DVDouble "" (freqJit*fund)
                  , DVDouble "" amplJit
                  , DVDouble "" jitCpsMin
                  , DVDouble "" jitCpsMax
                  , DVDouble "" testHpCo
                  ] 


{-

goals

  quickly test variations in the following

    individual sine waves (method 2) vs. full tables (method 1)

    with individual sine waves

      randomized start phase, or not

      presence of jitter, or not

      envelope on each sine wave, or same envelope on all

    algorithm for determining strengths of partials, and which partials are
    present or absent, that can be used with either method

      phase question is tricky. assume with tables we use GEN09 and specify
      spread phase

    with method 2

      method of creating stretch tuning, or not

      method of computing envelope for each wave

  general purpose routines

    determining strength of partials, and phase at 

      what do we need to determine strength?

        list of partial numbers, frequency of each one. note that some
        strengths might be zero

      1/n, 1/n**c, 2 out of 3 high partials omitted but other ones put at
      higher energy

      only need one routine. 

        takes Int (max partial num) and [(Int,Double)] (list of partial
        numbers and frequencies)

        returns [(Double,Double)] (strength of each partial and start phase)

    determine envelopes

      in case same envelope is on all, compute what that is

        probably refer to config file. outputs (Double,Double,Double) which is
        attack, decay rate, and tail

      in case individual envelopes, what do we need?

        attack, decay, and tail all will vary by frequency. I think I probably
        need frequency only. I can't think of another way to do it

        this routine will only be called by method 2, so it makes sense to
        call it from a high-level routine

        routine takes [(Int,Double)] so we can match other one. outputs
        [(Double,Double,Double)] which is list of attack, decay rate, and tail

    at top level, we need to come up with tables that should be present in the
    config. with method 2 that's just a sine. with other method we write
    tables.txt.

      instrument picks table? 

    writing a GEN09 table. needs [(Int,Double,Double)] which is partial
    number, strength, and phase from 0 to 1. needs table size also. oh needs
    midi pitch.

    some common patterns in writing sco.sco

      need to know final end time

      for a midi event, need to know time, dur, amp, pitch


-}

