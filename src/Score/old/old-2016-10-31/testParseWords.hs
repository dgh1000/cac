

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Score.ParseWords
import Score.ScoreData
import Common.CommonExport
import Common.CommonUtil
import Util.Parse
import Util.Map
import Data.Maybe
import Data.Map(Map)
import Data.Ratio

{-
parseTempo2 :: Parser Mark
parseTempo2 = do
  char '='
  (try (do i <- parseInt
           eof
           return $ TempoMark $ AbsTempo i)
   <|>
   try (do s <- many $ oneOf "-+0123456789."
           eof
           case reads s of 
             (x,[]):_ -> return . TempoMark . TempoRatioDouble $ x  
             _        -> fail "")
   <|>
   try (do i1 <- parseInt
           char ':'
           i2 <- parseInt
           eof
           return $ TempoMark $ TempoRatioFrac i1 i2)
   <|>
   try (do c <- oneOf "+-"
           s <- many1 digit
           char '%'
           eof
           case reads s of 
             (x,[]):_ -> do let n = if c == '-' then 100-x else 100+x
                            return $ TempoMark $ 
                              TempoRatioDouble ((fromIntegral n)/100)))
-}


timeSigs_1 = M.fromList
  [ (1, TimeSig 4 4)
  , (2, TimeSig 4 4) 
  , (3, TimeSig 4 4)
  , (4, TimeSig 4 4)
  , (5, TimeSig 4 4)
  , (6, TimeSig 4 4)
  , (7, TimeSig 4 4)
  , (8, TimeSig 4 4)
  , (9, TimeSig 4 4)
  ]


data_1 = listToLMap 
  [ (Loc 1 1, "a=100"    )
  , (Loc 1 (1+1%2), "w"   )
  , (Loc 1 2, "<1%4<"     )
  , (Loc 1 3, ">1%20>"    )
  , (Loc 1 4, "<1%20"     )
  , (Loc 2 1, ">+%20>"    )
  , (Loc 2 2, "<3%2;%8"   )
  , (Loc 2 3, "<3%2;1%2"  )
  , (Loc 2 4, "<%4"       )
  , (Loc 2 (4+1%2), "w"   )
  , (Loc 3 1, "T=100"    )
  , (Loc 3 2, "T=3:2a"    )
  , (Loc 3 3, "T=3:2"     )
  , (Loc 3 4, "$a"        )
  , (Loc 4 1, "arp@5"     )
  , (Loc 4 2, "stac@5")
  , (Loc 4 3, "ps@5")
  , (Loc 4 4, "ps%15")
  , (Loc 4 (4+1%2), "ps1%4")
  , (Loc 5 1, "a=100")
  , (Loc 5 2, "a=3:2b")
  , (Loc 5 3, "rit.")
  , (Loc 5 4, "accel.")
  , (Loc 6 1, "pat:foo")
  , (Loc 6 2, "wl{<+1%2;%/1<}")
  , (Loc 6 3, "wl!16")
  , (Loc 6 4, "wl!2")
  , (Loc 7 1, "1--" )
  , (Loc 7 2, "1:2--")
  , (Loc 7 3, "--2--")
  , (Loc 7 4, "--2:3--")
  , (Loc 8 1, "--2:3|4:5--")
  , (Loc 8 2, "--4:5")
  , (Loc 8 3, "--4:5=")
  , (Loc 8 (3+1%2), "DXF")
  , (Loc 8 4, "chan 1 2")
  ]


test :: Map Loc [String] -> IO ()
test words = do
  let marks = wordTesting timeSigs_1 words
      output = concatMap doLoc $ M.toAscList marks
      doLoc :: (Loc,[Mark]) -> String
      doLoc (loc,ms) = printf "%s\n" (showLoc2 loc) ++ concatMap doMark ms
      doMark m = printf "    %s\n" (show m)
  putStrLn output    


main = test data_1
    


-- warp
--
-- new ideas: can have spaces.
--
-- don't need w.
-- 
-- fraction of beat is indicated by %.
--
-- ratio can be expressed with EITHER : or / 
--
-- how to express tens of milliseconds? @ sign? yeah
--
-- <-1%2|
-- <-|1%2
-- <-s 1%2|
-- |s 1%2|->


-- accel
--
-- x=1   <+ 3:2
         





     

{-
x1 :: [(Int,String)]
x1 = reads "20"



parseShow :: Show a => Parser a -> String -> String
parseShow f s = case parse f "" s of
  Left x  -> show x
  Right x -> show x

-}

{-
xMarkMap = M.fromList [ (Loc 1 1, XMark Nothing)
                      , (Loc 5 1, XMark Nothing) ]
-}


{-
ioParseMany :: Show a => Parser a -> [String] -> IO ()
ioParseMany p ss = putStrLn . unlines . map (parseShow p) $ ss


tempoStrings = [ "a"
               , "1:2a"
               , "5:4"
               , "-"
               ]


warpStrings = [ "<1%4<"
              , ">1%20>"
              , "<1%20"
              , ">%20"
              , ">+%20>"
              , "<3%2;%8"
              , "<3%2;1%4<"
              , "<%4"
              ]



wordStrings = [ "T=100Q"
              , "T=3:2a"
              , "T=3:2"
              , "$a"
              , "arp@5"
              , "stac@5"
              , "ps@5"
              , "ps%15"
              , "a=100Q"
              , "a=3:2b"
              , "rit."
              , "accel."
              , "pat:foo"
              ]

rampStrings = [ "3:2>>"
              , "<<1:2"
              , ">>"
              , "<<1:2="
              , "<<100Q="
              , "<<2:1>>"
              , "<<4:5"
              , "<<2:1a"
              , "<<100Q>>"
              ]


wMarks = S.fromList [Loc 2 1, Loc 4 1]


timeSigs = M.fromList [ (1, TimeSig 4 4)
                      , (2, TimeSig 4 4)
                      , (3, TimeSig 4 4)
                      , (4, TimeSig 4 4)
                      , (5, TimeSig 4 4)
                      ]


main1 = ioParseMany (simpleRampBegin <|> rampBegin <|> rampEnd) rampStrings


main2 = do
  -- configuration
  let simulatedWarpLoc = Loc 3 1

  let one s = do 
        case parse warp "" s of
          Left err -> print err
          Right w  -> 
            print $ snd $ fromWarpTemp timeSigs wMarks (simulatedWarpLoc,w)
  mapM_ one warpStrings


main3 = ioParseMany word rampStrings
-}
