module Util.FileUtil ( mostRecentFile, mostRecentFileNDirs, availableFilename
                     , writeFileStrictly, readFileStrictly
                     , treeFiles, mostRecentFileL ) where

-- import Text.Regex.Posix
import Text.Printf
import Data.Char
import Data.Maybe
import Data.Function
import System.FilePath
import Control.Monad
import qualified Data.List as L
import Data.List
import Debug.Trace
import System.Directory
import Util.Exception
import qualified Data.ByteString as B



----------------------------------------------------------------------
----------------------------------------------------------------------
--                 convert Strings to strict bytestrings for
--               wrint to files
writeFileStrictly :: FilePath -> String -> IO ()
writeFileStrictly fn s = B.writeFile fn . B.pack . map (fromIntegral . ord) $ s

readFileStrictly :: FilePath -> IO String
readFileStrictly fn = do
  f <- B.readFile fn
  return  $ map (chr . fromIntegral) . B.unpack $ f


----------------------------------------------------------------------
----------------------------------------------------------------------
--                     mostRecentFile


-- Given a directory path and an extension string, find the most
-- recent file in that directory with that extension.
--
-- Example: mostRecentFile "c:/Mike/Music/stuff" "xml"
--  This will find the most recent xml file.
mostRecentFile :: FilePath -> String -> IO FilePath
mostRecentFile path ext = do
  let pathSlash = path ++ "/"
  fs <- getDirectoryContents path
  let ffs = filter g fs
      g s = drop (length s - length ext) s == ext
  if null ffs 
    then error "In mostRecentFile: no file of given extension in given dir"
    else return ()
  ts <- mapM (getModificationTime . (pathSlash++)) ffs
  -- pathSlash: the path to directory we are examining with a slash added
  -- ts: [Timestamp or someting]  list of times
  -- ffs: filenames (in same order as ts)
  -- zip ts ffs: [(Timestamp,String)]
  -- maximum: tuple is instance of Ord
  return $ ((pathSlash++) . snd . maximum . zip ts) ffs


mostRecentFileNDirs :: [FilePath] -> String -> IO FilePath
mostRecentFileNDirs paths ext = do
  allFileTimes <- concat `liftM` mapM (directoryFileTimes ext) paths
  case allFileTimes of
    [] -> throwMine $ printf ("In mostRecentFileNDirs, no file of extension "++
          "'%s' in any of the given directories") ext
    xs -> return . snd . maximum $ xs


mostRecentFileL :: [FilePath] -> IO FilePath
mostRecentFileL fs = do
  ts <- mapM getModificationTime fs
  case zip ts fs of
    [] -> throwMine "call to mostRecentFileL with no files or dirs"
    xs -> return . snd $ maximumBy (compare `on` fst) xs 


directoryFileTimes ext path = do
  let extMatches s = takeExtension s == "." ++ ext
      getFileTime name =
        if extMatches name 
          then do let fullFilename = path ++ "/" ++ name
                  t <- getModificationTime fullFilename
                  return $ Just (t,fullFilename)
          else return Nothing
  flag <- doesDirectoryExist path
  if flag then do fs <- getDirectoryContents path
                  catMaybes `liftM` mapM getFileTime fs
          else return []


----------------------------------------------------------------------
----------------------------------------------------------------------

treeFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
treeFiles pred fp = do
  contents  <- map (\f -> joinPath [fp,f]) `liftM` listDirectory fp
  fileFlags <- mapM doesFileExist contents
  dirFlags  <- mapM doesDirectoryExist contents
  let files = map snd . filter (pred . snd) . filter fst $
              zip fileFlags contents
      dirs  = map snd $ filter fst $ zip dirFlags  contents
  x <- mapM (treeFiles pred) dirs
  let y :: [FilePath]
      y = concat x
  return $ files++y
  
  -- ((files++) . concat) `liftM` mapM treeFiles dirs

----------------------------------------------------------------------
----------------------------------------------------------------------
--            availableFilename

-- Compute a file name that is "available" (no current file by that name)
--   by appending two digits to a base name.
--
-- Inputs
--   FilePath :: the directory to look in
--   String :: the prefix of the desired file name. Two digits will be
--     added to it to make an available file. For instnce, if the base name
--     is "foo" and the extension is "txt", then this program will look for
--     an available filename of a form like foo01.txt, foo02.txt, foo03.txt, 
--       etc.
--   String: the extension of the desired filename
availableFilename :: FilePath -> String -> String -> IO FilePath
availableFilename path prefix ext = do
  fs <- getDirectoryContents path
  let ls = catMaybes . map (avfn_help prefix ext) $ fs
  let currentMax = maximum $ 0 : ls
      newFn = prefix ++ show (currentMax + 1) ++ "." ++ ext
  when (newFn `elem` fs) (throwMine "987sdf")
  return newFn

-- Check filename for the form <prefix><numbers>.<ext>
--
-- Inputs
--   String :: prefix that it might match
--   String :: extension that it should have
--   String :: filename to check
-- Output
--   Nothing:: if file doesn't meet the form
--   Just <numeric value of numbers>
avfn_help :: String -> String -> String -> Maybe Int
avfn_help prefix ext name =
  if ext == nameExt
  then
    case checkPrefix prefix nameBase of
      Nothing -> Nothing
      Just remainder -> case reads remainder of
        [] -> Nothing
        ((i,_):_) -> Just i
  else Nothing
  where
    (nameBase,nameExt) =
        let (b,e) = splitExtension name
        in (b, drop 1 e)
    

-- checkPrefix
-- 
-- Inputs:
--   - prefix to check
--   - list to check: does this have the prefix at its beginning?
-- Outputs
--   - Nothing if the list to check doesn't have the given prefix
--   - Just the remainer of the list following the prefix
checkPrefix :: Eq a => [a] -> [a] -> Maybe [a]
checkPrefix possiblePrefix listToCheck 
  | lenPossPre == 0 || lenListToCheck == 0 = throwMine "098v"
  | lenListToCheck <= lenPossPre = Nothing
  | otherwise =
    if L.isPrefixOf possiblePrefix listToCheck 
    then Just $ drop lenPossPre listToCheck
    else Nothing
  where
    lenPossPre = length possiblePrefix
    lenListToCheck = length listToCheck

----------------------------------------------------------------------
----------------------------------------------------------------------


{-

           removed May 2017 because I started building with stack/cabal and
had trouble setting up regular expressions




-- backupSystem
--  Inputs
--    FilePath:: full (absolute) path to file to be backed up. 
--    FilePath:: path path to backup directory there
--  
--  Outputs
--     Bool :: True if succeeded; false if not (if not success, error msg will
--           be written to stdout)
--
backupViaNumbers :: FilePath -> FilePath -> IO Bool
backupViaNumbers origFilePath backupDirectory = do
  let (_,origFileName) = splitFileName origFilePath
      (origFileBaseName, origFileExtWithDot) = splitExtension origFileName
  when (length origFileExtWithDot < 2) (throwMine "dflj34k")
  let origFileExt = drop 1 origFileExtWithDot
  bdContents <- getDirectoryContents backupDirectory
  let ns = catMaybes $ map (_checkForExistingBackupFileName origFileBaseName
        origFileExt) bdContents
      newNumber = maximum (0:ns) + 1
      backupName = origFileBaseName ++(show newNumber) ++ origFileExtWithDot
      backupWholePath = backupDirectory </> backupName
  copyFile origFilePath backupWholePath
  return True
  

_checkForExistingBackupFileName :: String -> String -> String -> Maybe Int
_checkForExistingBackupFileName base ext fname = out
  where
    pat = printf "%s([0-9]+)\\.%s" base ext :: String
    things = fname =~ pat :: [[String]]
    out = case things of
       [] -> Nothing
       [_,ds]:_ -> Just (read ds)

-}
