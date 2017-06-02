import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary
import Data.Int
import System.IO
import Control.Monad
import Util.MiscCsound.HetroFormat.Hetro

main = do
  HetroData n rows <- readHetFile "example.het"
  putStrLn $ "n partials: " ++ show n
  let HetroRow typ values = rows !! 5
  print $ take 10 values
  -- print $ map fst stuff

main2 = do
  hd <- readHetFile "example.het"
  putStrLn $ showSomeHetData hd
