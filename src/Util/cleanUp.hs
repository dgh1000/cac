
import System.Posix
import Data.Either
import Data.Maybe
import Control.Monad
import System.Directory
import System.FilePath


main = do
  let topDir = "c:/mike/music/compositions/"
  con <- getDirectoryContents topDir
  let conFull = map (\s -> joinPath [topDir,s]) con
  (fs,dirs) <- partitionPaths conFull
  putStrLn "Files:"
  mapM_ (\s -> putStrLn ("  " ++ s)) fs
  putStrLn "Dirs:"
  mapM_ (\s -> putStrLn ("  " ++ s)) dirs
  print doOneDir topDir
  -- size <- doOneDir topDir
  -- print size


doOneDir :: FilePath -> IO Int
doOneDir d = do
  contents <- getDirectoryContents d
  (files, dirs) <- partitionPaths contents
  sizes <- mapM getFileSize baks
  -- dirs < - getDirectoryContents dirs
  -- subsSizes = mapM doOneDir dirs
  return $ sum sizes


{-
isBak :: String -> Bool
isBak = (== ".bak" ) . takeExtension
-}

getFileSize :: String -> IO FileOffset
getFileSize path = do
  stat <- getFileStatus path
  return $ fileSize state

partitionPaths :: [FilePath] -> IO ([FilePath],[FilePath])
partitionPaths ss = do
  things <- forM ss (\s -> do 
    f <- doesFileExist s
    d <- doesDirectoryExist s
    case (f,d) of
      (True, False) -> return $ Left s
      (False, True) -> return $ Right s
      otherwise -> error $ "huh?" ++ s)
  return $ partitionEithers things

