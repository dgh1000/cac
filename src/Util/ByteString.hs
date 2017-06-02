
module Util.ByteString where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL


lazyToStrict :: BL.ByteString -> B.ByteString
lazyToStrict = B.pack . BL.unpack
 