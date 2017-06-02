
import Data.Maybe
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Csound.Instruments.Gen10
import Util.Showable

t1 = M.fromList [(1,1)]
t2 = M.fromList [(2,2)]

gen10DataFile = "/Mike/Music/algo/haskell/Csound/Instruments/configurations/gen10Violin.bin"

main = do
  let sas = WaveformAttack [1,2,3] [t1,t1,t1]
      fas = WaveformAttack [4,5,6] [t1,t2,t1]
      srs = WaveformRelease [1,3,6] [t2,t2,t2]
      ss = t2
  BL.writeFile "m40_sas.spc" (encode sas)
  BL.writeFile "m40_fas.spc" (encode fas)
  BL.writeFile "m40_srs.spc" (encode srs)
  BL.writeFile "m40_ss.spc" (encode ss)

{-

      op = OnePitch ss sas fas srs
      data_ = Gen10Data $ M.fromList [(40,op)]
      BL.writeFile encode data_
  
-}
      

main2 = do
  binary <- BL.readFile gen10DataFile
  let Gen10Data d = decode binary :: Gen10Data
  print $ fromJust $ M.lookup 40 d
  -- putStrLn $ show (decode binary :: Gen10Data)

showGen10Data = do
  binary <- BL.readFile gen10DataFile
  let g = decode binary :: Gen10Data
  writeFile "tmp.txt" (showItem . showi $ g)

