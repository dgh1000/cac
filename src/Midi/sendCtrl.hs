
import Text.Parsec
import Text.Parsec.String
import System.Environment
import Control.Monad
import Data.Maybe
import Midi.Interface
import Sound.PortMidi
import Util.Exception


-- need several Moog instruments, at least one for each wiring configuration
-- we want to use. pipe 1 is roland, pipe 2 is sibelius sending pitches when
-- input, pipe 3 through 5 

main = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  eStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  let streams = case eStreams of
        Left err -> throwMine $ show err
        Right s  -> s

  [arg] <- getArgs
  let (streamNum,chan,ctrl) = case parse streamChanCtrl "" arg of
        Left err -> throwMine $ show err
        Right x  -> x
  writeShort (streams !! streamNum) $ toPMEvent (0xB0+chan-1,ctrl,64)


streamChanCtrl = do
  ds1 <- many1 digit
  char ':'
  ds2 <- many1 digit
  char ':'
  ds3 <- many1 digit
  return (read ds1,read ds2,read ds3)
