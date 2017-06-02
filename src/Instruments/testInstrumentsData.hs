
import qualified Data.Map as M
import System.Random
import Control.Monad.State
import Control.Monad.Reader
import Score.ScoreData
import Instruments.InstrumentsData
import Instruments.ShowInstruments
import Translation.TranslationData
import Util.Showable


tv = TimingVariation 0 0 0 0 0 0 

trs gen = TrState emptyScore M.empty tv gen M.empty
          (SMap M.empty) (SMap M.empty) (SMap M.empty) [] []


im :: I Salue
im = do
  -- staffState ... "JacyMap" === SMap M.empty
  staffState ... "JacyMap" ... "a double" `iSet` SDouble 3.0
  staffState ... "JacyMap" ... "another double" `iSet` SDouble 4.0
  lk $ staffState ... "JacyMap" ... "a double" 


tm :: Tr Bool
tm = do
  v <- runReaderT im (Meta "foo" (return ()) [] (\_ -> return ()))
  staffState ... "LindaMap" ... "x" ... "y" `trSet` SDouble 0.0
  staffState ... "JacyMap"  ... "x" ... "y" `trSet` SDouble 3.1415
  staffState ... "JacyMap"  ... "x" ... "y" `trXform` xform 2
  staffState ... "JacyMap"  ... "a double" `trXform` xform 1
  exists $ staffState ... "JacyMap" ... "x" ... "y"


xform x (SDouble y) = (SDouble $ x+y)


main = do
  gen <- newStdGen
  let (v,endState) = runState tm (trs gen)
  putStrLn $ showIString $ tsStaffState endState
  putStrLn $ show v
 
