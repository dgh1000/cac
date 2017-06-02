{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Data.Array.Unboxed
import qualified Data.Map as M
import Data.Map(Map)
import Text.Printf
import qualified Data.List as L
import Text.XML.Light
import XmlDoc.ParseXml
import XmlDoc.XmlDocData
import Score.XmlToScore
import Score.ShowScore
import Score.ScoreData
import Common.CommonExport
import Common.CommonUtil
import Util.FileUtil
import Util.Showable
import Util.Exception

readXml :: IO Score
readXml = do
  buf <- readFileStrictly "/Users/Mike/out.xml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e


makeSlurData (loc,ns) =
  (loc, [ XMDNote (XNNote 1 False False Nothing Nothing
                          (XPitch "A" 0 0) False False ns Nothing)
        ]
  )        


n1 = (Loc  1  1, [XNSlur "start" Nothing])
n15 = (Loc 1 3, [XNSlur "stop" (Just 2), XNSlur "stop" Nothing])
n2 = (Loc  2  1, [XNSlur "start" Nothing ])
n3 = (Loc 3 1, [XNSlur "stop" Nothing])


ms :: Map Loc [XMsrData]
ms = M.fromListWith (++) $ map makeSlurData [n1,n15,n2,n3]

{-

type LocLoc = (Loc,Loc)

instance ShowItemClass LocLoc where
  showI (loc1,loc2) = SingleLine $ printf "%s %s" (showLoc2 loc1) (showLoc2 loc2)



main = putStrLn $ showiToString $
         Component "foo" False (map showI . M.toAscList $ computeSlurs ms)
         
-}


main = do
  score <- readXml
  writeFile "out.txt" . showIString $ score


main2 = myOrdering 3 [(3,"nodin"),(2,"jacy"),(3,"keary")]

main3 = myOrdering2 "jacy" [(3,"nodin"),(2,"jacy"),(3,"keary")]



