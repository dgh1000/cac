import Control.Monad
import Sound.PortMidi
import Text.XML.MusicXML hiding (String,State,StateT)
import Text.XML.MusicXML.Partwise 

import Util.FileUtil
import Util.Env
import MusDoc.Data
import MusDoc.Processing
import MusDoc.FromXml
import Midi.FromMusDoc
import Midi.Operate


main = do
  -- Configuration
  let dir = "c:/Mike/Music/algo/compositions/finale/2010/"
      -- dir = "c:/Mike/Music/bach_midi/sinfon"
  
  putStrLn $ "Looking in directory \n  " ++ dir
  filename <- mostRecentFile dir "xml"
  putStrLn $ "Using file\n  " ++ filename
  result <- read_FILE read_MusicXML_Partwise filename 
  if isOK result 
    then run $ (toMusDoc . fromOK) result
    else print $ fromError result

run :: MusDoc -> IO ()
run doc = do
  -- Open midi device
  initialize
  midiDev <- defaultMidiDevice
  result <- openOutput midiDev 1000
  case result of
    Left stream -> do
      play stream doc
      close stream
      return ()
    Right err -> do
      print err
  terminate
  return ()
  
play :: PMStream -> MusDoc -> IO ()
play stream doc = do
  let verts = computeVertLocs doc
  -- to play we need start and end loc, voices doc stream
  mapM_ (playOne stream doc) verts
  
playOne :: PMStream -> MusDoc -> Loc -> IO ()
playOne stream docIn loc = do
  let docOut = convenientTrim loc loc docIn
      evts = toMidi docOut
  case evts of
    [] -> putStrLn "Nothing to play."
    _  -> sendMidi stream evts 200 1000
      
{-           
-- Play composition.
--   Int             Count of verticals to play.
--   [Int]           Midi channels to include (if this is an empty 
--                    list, use all channels)
playIt :: Int -> [Int] -> IO ()
playIt count voices = do
  PlayState verts len docIn stream pos1 <- get
  let pos2 = min (pos1+count-1) (len-1)
      loc1 = verts !! pos1
      loc2 = verts !! pos2
      loc3 = minExtendBeyond loc1 loc2 docIn 
      -- Get rid of notes that start after loc2
      docV2 = filterDoc (\Note{MD.loc=loc} -> loc<=loc2) docIn
      docOut = snipDoc loc1 loc3 docV2
      evts = toMidi $ case voices of
        [] -> docOut
        _ -> filterMidiChans voices docOut
  case evts of
       [] -> liftIO $ putStrLn "Nothing to play."
       es -> liftIO $ sendMidi stream es 200 1000

-}
