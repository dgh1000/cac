
import Text.Printf
import Util.Exception


makeSin :: Int -> Int -> Double -> Double -> [Double]
makeSin size partial strength phase = 
    map 
      (\x -> strength * 
        (sin $ 2 *pi*fromIntegral partial*
          (fromIntegral x+phase*sf)/sf
        )
      )
      [0..size-1]
  where
    sf = fromIntegral size

makePartials :: Int -> [(Int,Double,Double)] -> [Double]
makePartials size partials = 
  foldr1 (zipWith (+)) $ map (\(n,s,p) -> makeSin size n s p) partials
  where
    l = length partials



main = do
  let h maxPartial n = (n, 1/fromIntegral n,fromIntegral n/fromIntegral maxPartial + 0.5)
      pts = makePartials 1000 $ map (\x -> h 250 x) [1..250]
      g :: (Int,Double) -> String
      g (x,y) = printf "%d %.5f\n" x y
  writeFile "plot.txt" (concatMap g . zip [1..] $ pts)


