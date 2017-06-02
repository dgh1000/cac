
import System.IO
import System.Process
import System.FilePath
import System.Console.Haskeline
import GHC.IO.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Debug.Trace
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Util.FileUtil
-- import Language.Haskell.Interpreter



main = runInputT defaultSettings $ loop Nothing


loop :: Maybe ProcessHandle -> InputT IO ()
loop mProcHand = do
  mL <- getInputLine "spawn > "
  case mL of
    Nothing -> loop mProcHand
    Just l  -> do
      (flag,mPh) <- liftIO $ loopIO l mProcHand
      if flag then return () else loop mPh


loopIO :: String -> Maybe ProcessHandle -> IO (Bool,Maybe ProcessHandle)
loopIO l mProcHand = do
  case parse cmdLine "" l of
    Left err -> putStrLn ("parse error: " ++ show err)
                  >> return (False,mProcHand)
    Right cmd -> case cmd of
      Play (beg,mEnd) mSolo -> do
        -- file name is found by searching tree
        fp <- treeFiles ((== ".sib") . takeExtension)
                "/Users/Mike/Dropbox/music/comp"
                >>= mostRecentFileL
        let fpNoExt = takeDirectory fp ++ "/" ++ takeBaseName fp
        flag <- compile (fp -<.> ".hs")
        if flag
           then do
             doStop mProcHand
             ph <- spawn fpNoExt (PlayCmd beg mEnd mSolo)
             return (False,Just ph)
           else return (False,mProcHand)
      Quit  -> doStop mProcHand >> return (True,Nothing)
      Stop  -> doStop mProcHand >> return (False,Nothing)
      Blank -> return (False,mProcHand)
      SendCtrl str chan ctrl ->
        doStop mProcHand >> spawnCtrl str chan ctrl >> return (False,Nothing)


doStop :: Maybe ProcessHandle -> IO ()
doStop mProcHand = case mProcHand of
  Nothing -> return ()
  Just h -> do
    c <- getProcessExitCode h
    case c of
      Just _ -> return ()
      Nothing -> interruptProcessGroupOf h >> return ()
  

compile :: FilePath -> IO Bool
compile fp = do
  -- 
  let c = printf "stack ghc --stack-yaml $DB/stack/cac/stack.yaml %s" fp
  (_,_,_,h) <- createProcess (shell c)
  exit <- waitForProcess h
  putStrLn ""
  return $ exit == ExitSuccess

    
spawn :: FilePath -> PlayCmd -> IO ProcessHandle
spawn execName cmd = do
  let cp = CreateProcess (RawCommand execName [showPlayCmd cmd])
        Nothing Nothing CreatePipe Inherit Inherit False False True False
        False False Nothing Nothing
  (_,_,_,ph) <- createProcess cp
  return ph


spawnCtrl str chan ctrl = do
  let execName = "/Users/Mike/haskell/Midi/sendCtrl"
  callProcess execName [printf "%d:%d:%d" str chan ctrl]
  
----------------------------------------------------------------------
--                    parsing command line



data PlayCmd = PlayCmd Int (Maybe Int) (Maybe String)


showPlayCmd (PlayCmd mBeg mEnd mSolo) =
  printf "%d-%d{%s}" mBeg (case mEnd of {Nothing -> 0; Just m -> m})
                          (case mSolo of {Nothing -> ""; Just s -> s})


data CmdLine = Play (Int,Maybe Int) (Maybe String)
             | SendCtrl Int Int Int -- stream, chan, ctrl
             | Quit
             | Stop
             | Blank
             | Redo


cmdLine :: Parser CmdLine
cmdLine = play <|> quit <|> stop <|> blank <|> redo <|> sendCtrl


play = do
  ds1 <- many1 digit
  ds2 <- optionMaybe $ char '-' >> many1 digit
  many space
  soloMeta <- optionMaybe $ between (char '[') (char ']')
                                     (many1 $ noneOf "[]") 
  return $ Play (read ds1,read <$> ds2) soloMeta

quit = char 'q' >> eof >> return Quit

stop = char 's' >> eof >> return Stop

blank = eof >> return Blank

redo = char 'r' >> eof >> return Redo


sendCtrl = do
  try $ string "ctrl"
  strNum  <- many1 digit
  char ':'
  chanNum <- many1 digit
  char ':'
  ctrlNum <- many1 digit
  eof
  return $ SendCtrl (read strNum) (read chanNum) (read ctrlNum)
