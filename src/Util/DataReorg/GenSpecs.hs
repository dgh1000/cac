module Util.DataReorg.GenSpecs where

import Text.Parsec
import Text.Parsec.String 
import System.FilePath
import System.Directory
import Control.Monad
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as L
import Text.Printf
import Util.Map
import Util.Exception




{-

plan for checking that all files have been included in reorganized data

  idea is that every file found in source directory list S should be found in
  destination D

    there may be some exceptions that are okay

      files in S that are missing from D

        we need to describe these exceptions somehow. one thing would be to
        list full path of file in S. another thing would be to list directory
        in S in which every file is an exception. another thing would be
        wildcards, say specify exceptions somehow

    how do we identify a file

      filename, if it is the only such name in S and the only such name in
      D. also size and date.

      is filename, size, and date ever not workable?

        let's say we've changed a file size and date since moving to D. our
        program can match the name N. 


      

-}

----------------------------------------------------------------------
----------------------------------------------------------------------



----------------------------------------------------------------------
----------------------------------------------------------------------
--                          entry points

genSpecs :: String -> FilePath -> IO ()
genSpecs root outputFile = do
  ss <- produceList root
  L.writeFile outputFile $ TL.encodeUtf8 . TL.concat $ ss



----------------------------------------------------------------------
----------------------------------------------------------------------

data FileType = NormalType | OneDriveWeirdType | AdobeWeirdType



parseFileType :: String -> FileType
parseFileType s = case parse fileType "" s of
  Left err -> throwMine $ printf "parse error on filename '%':%s" s (show err)
  Right t -> t


fileType :: Parser FileType
fileType = do
  (try $ do char '.'
            many1 (oneOf "-ABCDEF0123456789")
            eof
            return OneDriveWeirdType)
  <|> 
  return NormalType


adobe :: Parser String
adobe = try $ string "Adobe Premiere Elements"

----------------------------------------------------------------------
----------------------------------------------------------------------

toByteString :: String -> L.ByteString
toByteString = TL.encodeUtf8 . TL.pack


-- Given the FULL PATH to a file, return a string describing it, including
-- modification time, directory, and file name.
fileSpec :: FilePath -> IO (Maybe TL.Text)
fileSpec f = do
  let nameString = takeFileName f
      nameText = TL.pack $ takeFileName f
      dirText = TL.pack $ takeDirectory f
  case parseFileType nameString of
    OneDriveWeirdType -> return Nothing
    NormalType        -> do
      timeText <- (TL.pack . show) `liftM` getModificationTime f
      return $ Just $ TL.concat 
        [timeText,TL.pack "|",nameText,TL.pack "|",dirText,TL.pack "|\n"]



{-
-- Given input directory 'dir', make a list of all subdirectories, add 'dir'
-- to the list, and return the list.
wholeTree :: FilePath -> IO [FilePath]
wholeTree dir = do
  subdirs <- listSubDirectoriesFullPath dir
  subDirTrees <- concat `liftM` mapM wholeTree subdirs
  return $ dir:subDirTrees


listSubDirectoriesFullPath :: FilePath -> IO [FilePath]
listSubDirectoriesFullPath dir = do
  let g :: FilePath -> IO (Maybe FilePath)
      g possible = do
        flag <- doesDirectoryExist possible
        if flag then return $ Just (dir++"\\"++possible)
                else return Nothing
  entries <- listDirectory dir
  catMaybes `liftM` mapM g entries
-}


-- produceList
--
--   Look at existing files in a directory tree: read their names, paths, and
--   modification times. For each file produce a string, then return the list
--   of those strings.
produceList :: FilePath -> IO [TL.Text]
produceList root = do
  contents <- listDirectory root
  let g :: FilePath -> IO [TL.Text]
      g dirEntry = do 
        let fullPath = root ++ "\\" ++ dirEntry
        flag1 <- doesDirectoryExist fullPath
        flag2 <- doesFileExist fullPath
        case (flag1,flag2) of
          (True , False) -> produceList fullPath
          (False, True ) -> do
            s <- fileSpec fullPath
            case s of
              Nothing -> return []
              Just x  -> return [x]
  concat `liftM` mapM g contents


