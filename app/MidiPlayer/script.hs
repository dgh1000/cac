
import System.Directory
import System.FilePath
import Text.Printf
import qualified Data.Map as M
import Data.Map(Map)
import Util.FileUtil

t1 = [(1,'a'),(1,'b'),(2,'x')]
t2 = foldr t3 M.empty t1
t3 :: (Int,Char) -> Map Int String -> Map Int String
t3 (i,s) = M.insertWith (++) i [s]


t4 :: String
t4 = printf "%02x" (11 :: Int)

d1 = "c:\\Users\\Mike\\crit\\music\\compositions\\2011\\viola"

main = do
  c <- getCurrentDirectory
  f <- mostRecentFile d1 "xml"
  let (_, s2) = splitFileName f
  let act = joinDrive "g:\\" s2
  print act
