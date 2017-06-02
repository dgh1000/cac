

module PlaybackCac.PlaybackCac(playCompToCsound) where

import Util.MonadStacks
import Cac.CacData
import Util.RandomState
import Playback.PlaybackUtil
import CacToCsound.CacToCsound
import CacToCsound.CacToCsoundData
import Csound.CsoundNotesToCsd

playCompToCsound :: CacToCsMap -> ErrorRand Comp -> IO ()
playCompToCsound conv theComputation = do
  let masterOrcFN = "c:/Users/Mike/crit/Music/algo/haskell/" ++
                     "CacToCsound/masterOrc.csd"
      options = "-d -W -o/temp/out.wav"
  masterOrc <- readFile masterOrcFN
  rd <- freshRandData
  case runER theComputation rd of
    ((Left err,_),_) -> putStrLn err
    ((Right comp,log),_) -> do
       -- putStrLn . showItem . showi $ nodes
       putStrLn log
       case runEW (compToCsoundCsd conv masterOrc options comp) of
         (Left err,_) -> putStrLn err
         (Right csd,log) -> do
           putStrLn log
           playCsd csd
           -- justWriteCsd csd
           putStrLn "Done."

compToCsoundCsd :: CacToCsMap -> String -> String -> Comp -> ErrWri String
compToCsoundCsd conv masterOrc options comp = do
  cnotes <- compToCsoundNotes conv comp
  csoundNotesToCsd masterOrc options cnotes

playCsd :: String -> IO ProcessHandle
playCsd csd = do 
  fname <- computeCsdName Nothing
  -- let fname = "out.csd"
  writeFile fname csd
  putStrLn $ "Wrote " ++ fname
  cwd <- getCurrentDirectory
  runProcess "c:/Program Files/Csound/bin/csound.exe" [fname] (Just cwd)
    Nothing Nothing Nothing Nothing



-- 
computeCsdName :: Maybe String -> IO String
computeCsdName (Just s) = return s
computeCsdName Nothing = do
  let possibleNames = map (\d -> "out" ++ show d ++ ".csd") [1..9]
  cwd <- getCurrentDirectory
  fs <- getDirectoryContents cwd
  -- pns:: [(FilePath, Maybe ClockTime)]
  pns <- mapM (checkPossibleName fs) possibleNames
  let (pns1,pns2) = partition (\(_,mc) -> isNothing mc) pns
  case pns1 of
    x:_       -> return $ fst x
    otherwise ->
      let pns2s = sortBy (\(_,jt1) (_,jt2) -> compare (fromJust jt1)
                                              (fromJust jt2)) pns2
      in return $ fst $ head pns2s

checkPossibleName :: [FilePath] -> FilePath -> IO (FilePath, Maybe ClockTime)
checkPossibleName contents possibleName =
  if any (== possibleName) contents
  then do
    t <- getModificationTime possibleName
    return $ (possibleName, Just t)
  else return $ (possibleName, Nothing)

