
import Data.Char
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

readFileStrictly :: FilePath -> IO String
readFileStrictly fn = do
  f <- B.readFile fn
  return  $ map (chr . fromIntegral).  B.unpack $ f

main :: IO ()
main = do
  B.writeFile "foo2.txt" (B.pack . map (fromIntegral . ord) $ "log")

main2 = do
  s <- B.readFile "foo2.txt"
  print s

