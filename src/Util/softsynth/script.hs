
import Text.Printf

data CsoundConfig = CsoundConfig
  { cc09TableSize :: Int
  , ccBase        :: Double
  , ccFreqJit     :: Double
  , ccAmpJit      :: Double
  , ccMaxFreq     :: Double
  , ccMinAtt      :: Double
  , ccMaxAtt      :: Double
  , ccMinDecay    :: Double
  , ccMaxDecay    :: Double
  , ccMinTail     :: Double
  , ccMaxTail     :: Double
  }
                  deriving(Show,Read)


main = do
  b <- readFile "config.txt"
  case reads b of
    (x,_):_ -> print (x::CsoundConfig)
    _                     -> print "oops"


writeF = do
  let b = printf "f1 0 16385 -9 %s\n" values
      numPartials = 80 :: Int
      onePartial :: Int -> String
      onePartial n = printf "%d %.3f %.1f " n strength phase
        where
          strength = 1 / fromIntegral n**1.4 :: Double
          phase    = 360 * fromIntegral (n-1) /fromIntegral numPartials ::Double
      values = concatMap onePartial [1..80::Int]
  writeFile "temp.txt" b


