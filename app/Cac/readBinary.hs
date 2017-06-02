
import Data.Binary
import Util.Showable
import Data.ByteString.Lazy as BL hiding (writeFile)
import Cac.CacData


main = do
  let fn = "/Temp/comp.dat"
  buf <- BL.readFile fn
  let d :: Comp
      d = decode buf
  writeFile "/Temp/showReadVersion.txt" . showItem . showi $ d
