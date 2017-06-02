import qualified Data.ByteString as B
import Data.Char

readByteString :: Read a => B.ByteString -> a
readByteString = read . map (chr . fromIntegral) . B.unpack


main = do
  let bs = B.pack $ map (fromIntegral . ord) "123"
  -- let newl = fromIntegral $ ord '\n'
  print $ B.split (fromIntegral .ord $ '\n') bs
  let x = readByteString bs  :: Int
  print x
