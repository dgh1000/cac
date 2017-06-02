
import Data.Map(Map)
import qualified Data.Map as M
import Data.Char
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary
import Csound.CsoundData( LoadedInstrConfig(..)
                        , OneLoadedInstrConfig(..) )

main2 = do
  let stuff = "foo"
      stuffB = B.pack $ map (fromIntegral . ord) stuff
  B.writeFile "foo.txt" stuffB

data Thing = Thing Word8
             deriving (Show)

instance Binary Thing where
  put (Thing i) = putWord8 i
  get = Thing `fmap` getWord8

              

t1 = Gen10ImitateSamplerLoadedConfig 0.1 
  [ (0.1, "8192 10 1") 
  ,  (0.2, "8192 10 1 1 1 1 1 1")
  ,  (0.3, "8192 10 1") 
  ,  (0.4, "8192 10 1 1 1 1 1 1")
  ,  (0.5, "8192 10 1")
  ,  (0.6, "8192 10 1 1 1 1 1 1")
  ,  (0.7, "8192 10 1")
  ]
t2 = LoadedInstrConfig $ M.fromList [("yeah", t1)]

main3 :: IO ()
main3 = do
  let s = encode (Thing 2)
  BL.writeFile "foo.bin" s
  s2 <- BL.readFile "foo.bin"
  let d :: Thing
      d = decode s2
  putStrLn (show d)

  
main :: IO ()
main = do
  let s = encode t2
  BL.writeFile "/Mike/Music/algo/haskell/Csound/instr-config/config.dat" s
  {- s2 <- BL.readFile ""
  let d :: LoadedInstrConfig
      d = decode s2
  putStrLn (show d)
  -}
