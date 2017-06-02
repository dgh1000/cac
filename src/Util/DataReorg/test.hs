
import Util.DataReorg.CmpSpecs
import Util.DataReorg.GenSpecs

main = do
  let srcRoot = "\\haskell\\Util\\DataReorg\\testSrc"
      srcSpecFile = "src.txt"
      dstRoot = "\\haskell\\Util\\DataReorg\\testDst"
      dstSpecFile = "dst.txt"
  genSpecs srcRoot srcSpecFile
  genSpecs dstRoot dstSpecFile
  cmpSpecs [srcSpecFile] [dstSpecFile]
  
----------------------------------------------------------------------
----------------------------------------------------------------------

fileReport :: FilePath -> IO ()
fileReport root = do
  genSpecs root "tmpSpecs.txt"
    

