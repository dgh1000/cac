
import qualified Data.Map as M
import Data.Map(Map)
import Data.Maybe
import Data.Ratio
import Common.CommonData
import Common.CommonUtil







{-
locNormalize :: Map Int TimeSig -> Loc -> Maybe Loc
locNormalize ts (Loc msr beat)
  | beat < 1 = case (fromIntegral . tsNumer) `fmap` M.lookup (msr-1) ts of
      Nothing    -> Nothing
      Just numer -> locNormalize ts (Loc (msr-1) (numer+beat))
  | otherwise = case (fromIntegral . tsNumer) `fmap` M.lookup msr ts of
      Nothing -> Nothing
      Just numer 
        | beat >= numer+1 -> locNormalize ts (Loc (msr+1) (beat-numer))
        | otherwise       -> Just $ Loc msr beat
-}

locs = [ Loc 2 0
       , Loc 1 0
       , Loc 3 (-4)
       , Loc 3 8   -- 3 3 is 4 1. add 4 and you get 3 7 and 4 5
       , Loc 3 3
       , Loc 4 5
       ]

addLocs = [ (Loc 2 1, 1)
          , (Loc 2 1, (4+1%2))
          , (Loc 2 1, 6)
          ]

subLocs = [ (Loc 1 1, 1) 
          , (Loc 1 2, 1)
          , (Loc 2 1, 1)
          , (Loc 3 1, 5)
          ]

x1 = locNormalize ts1 (Loc 2 0)
x2 = locNormalize ts1 (Loc 1 0)

ts1 = M.fromList [ (1,TimeSig 3 4)
                 , (2,TimeSig 4 2)
                 , (3,TimeSig 2 8)
                 , (4,TimeSig 4 4) ]

main = putStrLn . unlines . map (show . locNormalize ts1) $ locs

main2 = putStrLn . unlines . map (\(l,d) -> (show $ locAdd ts1 l d)) $ addLocs

main3 = putStrLn . unlines . map (\(l,d) -> (show $ locSub ts1 l d)) $ subLocs

-- beat 2: that's 4 quarters
main4 = putStrLn $ show $ locAddQuar ts1 (Loc 2 3) (6)
