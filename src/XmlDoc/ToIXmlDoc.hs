module XmlDoc.ToIXmlDoc where


import Data.Ratio
import Data.Maybe
import Data.List
import Data.Map(Map)
import qualified Data.Map as M
import XmlDoc.XmlDocExport
import qualified XmlDoc.XmlDocData as XD
import Common.CommonExport
import qualified Util.Map as UM

toIXmlDoc :: XScore -> IXmlDoc
toIXmlDoc XScore { XD.xParts = parts } =
  IXmlDoc 
  { XD.ixMsrInfos = mis
  , XD.ixParts = M.map toIXPart parts }
  where
    mis = case M.toList parts of {(_,p):_ -> computeMsrInfos p}
    toIXPart :: XPart -> IXPart
    toIXPart (XPart msrs) 
      = IXPart { XD.ixMsrDatas = msrDatas
               , XD.ixPartNotes = computePartNotes mis msrDatas }
      where
        msrDatas = computeMsrDatas mis msrs

        

computeMsrInfos :: XPart -> Map Int IXMsrInfo
computeMsrInfos (XPart msrs) = 
  case scanl step (Nothing,Nothing,Nothing) . map transformMsr $ msrs of
    _:ys -> M.fromList . zip [1..] . map toMsrInfo $ ys
  where
    combine x Nothing = x
    combine _ (Just x) = Just x
    step (a, b, c) (XMsrAttr ma mb mc) 
      = (a `combine` ma, b `combine` mb, c `combine` mc)
    toMsrInfo (Just x,Just y,Just z) = IXMsrInfo x y z
    transformMsr :: XMsr -> XMsrAttr
    transformMsr (XMsr _ (Just msrAttr) _) = msrAttr
    transformMsr _ = XMsrAttr Nothing Nothing Nothing

data TimeState = TimeState Int Int

accumTime :: [XMsrData] -> [(Int,XMsrData)]
accumTime = catMaybes . snd . mapAccumL f (TimeState 0 0)
  where
    f :: TimeState -> XMsrData -> (TimeState,Maybe (Int,XMsrData))
    f (TimeState currTime nextTime) d = case d of
      XMDBackup d -> (TimeState nextTime (nextTime-d), Nothing)
      XMDForward d -> (TimeState nextTime (nextTime+d), Nothing)
      x@(XMDNote n)
        | xnChord n -> ( TimeState currTime nextTime
                       , Just (currTime, x) )
        | otherwise -> ( TimeState nextTime (nextTime + xnDuration n)
                       , Just (nextTime, x) )
      x -> (TimeState currTime nextTime, Just (nextTime, x))
      
computeMsrDatas :: Map Int IXMsrInfo -> [XMsr] -> Map Loc [XMsrData]
computeMsrDatas mis msrs = M.fromListWith (++) . map (\(x,y) -> (x,[y])) .
                           concatMap f $ msrs
  where
    f :: XMsr -> [(Loc,XMsrData)]
    f m = map (\(d,x) -> (toLoc d,x)) . accumTime 
          $ XD.xmMsrDatas m
      where
        toLoc divs = case divsToLoc mis (XD.xmMsrNum m) divs of {Just x->x}

-- divsToLoc
--
--   Given the location of an event at a certain number of time divisions
--   past the beginning of a measure, calculate the Loc. The divisions may
--   equal the entire duration of the measure, in which case the Loc is
--   computed as the first beat of the next measure. 
--
--   If the number of divisions would put the Loc beyond beat 1 of the next
--   measure, Nothing is returned.
divsToLoc :: Map Int IXMsrInfo -> Int -> Int -> Maybe Loc
divsToLoc mis msrNum divs
  | b == fromIntegral numer + 1 = Just $ Loc (msrNum+1) 1
  | b < fromIntegral numer + 1 = Just $ Loc msrNum b
  | otherwise = Nothing
  where
    IXMsrInfo dpq numer denom = case M.lookup msrNum mis of
      Just x -> x
    b = 1 + (fromIntegral denom / 4) * (fromIntegral divs / fromIntegral dpq)

-- computePartNotes
--
--   extract the XNotes from the map of XMsrData, and also set the end
--   location at the same time
--
computePartNotes :: Map Int IXMsrInfo -> Map Loc [XMsrData] -> Map Loc [XNote]
computePartNotes mis = UM.listToLMap . catMaybes . map g . UM.lMapToList
  where
    g :: (Loc,XMsrData) -> Maybe (Loc,XNote)
    g (loc@(Loc locMsr locBeat), XMDNote n@(XNNote { XD.xnDuration=dur })) =
      Just (loc, n {XD.xnEndLoc = Loc endMsr endBeat})
        where
          IXMsrInfo dpq numer denom = case M.lookup locMsr mis of {Just x -> x}
          -- exhausted cases here means that the number of divs put the
          -- location past beat 1 of the next measure
          (endMsr,endBeat) | e == fromIntegral numer + 1  = (locMsr+1,1)
                           | e <  fromIntegral numer + 1  = (locMsr,  e)
            where
              e = locBeat + (fromIntegral denom / 4) * 
                  (fromIntegral dur / fromIntegral dpq)
    g _ = Nothing

{-
computeDirections :: Map Int IXMsrInfo -> Map Loc [XMsrData] ->
                     Map Loc [XDirection]
computeDirections mis = UM.mapListElemsMaybe g
  where
    g :: XMsrData -> Maybe XDirection
    g
-}
