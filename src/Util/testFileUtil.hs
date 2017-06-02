
import Control.Monad
import System.FilePath

import System.Directory
import Util.FileUtil

main = mostRecentFile "c:/Mike/music/algo/compositions/finale/2010" "xml"
main2 = availableFilename "c:/Temp" "xxx" "txt"
main3 = getDirectoryContents "c:/Temp"

main4 = do
  let d = "/Users/Mike/Dropbox/music/comp/2014/"
  cwd <- getCurrentDirectory
  print cwd
  let f x y = [x,y]
  doesDirectoryExist d >>= print
  listDirectory d >>= mapM doesDirectoryExist . map (joinPath . f d)
     >>= print
  fs <- treeFiles ((==".sib") . takeExtension) d
  print fs
  

t4 = do
  backupViaNumbers "/haskell/Util/Text.hs" "/haskell/Util/backup"

dirs = [ "c:/Users/Mike/OneDrive/trad-comp/2016/test"
       , "c:/Users/Mike/OneDrive/trad-comp/2016/tonal"
       ]


t5 = do
  name <- mostRecentFileNDirs dirs "sib"
  print name
