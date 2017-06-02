
import Util.Env

main = do
  p <- algoPath
  buf <- readFile (p ++ "/haskell/Util/Env.hs")
  writeFile "test.hs" buf
