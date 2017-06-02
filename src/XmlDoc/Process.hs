module XmlDoc.Process where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified XmlDoc.XmlDocData as XD
import qualified Util.Map as UM
import Text.Printf
import Debug.Trace
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Set(Set)
import Data.Map(Map)
import XmlDoc.XmlDocExport
import Common.CommonUtil
import Common.CommonData
import Util.Exception



----------------------------------------------------------------------
----------------------------------------------------------------------
--        making type Map Loc [XMsrData]  for each part

-- changes XML parts to Map Loc [XMsrData], also splitting parts with
-- multiple staves and eliminating rests.
computeXmlStaves :: XScore -> ( Map Int IXMsrInfo
                              , Map String (Map Loc [XMsrData]) )
computeXmlStaves (XScore _ parts) 
  = (mis, splitMsrDataMap . M.map g $ parts)
  where
    mis = case M.toList parts of {(_,p):_ -> computeMsrInfos p}
    g :: XPart -> Map Loc [XMsrData]
    g (XPart msrs) = computeMsrDatas mis msrs

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

data TimeState = TimeState Int Int (Maybe Int)  -- <prev time> <current time>
                                                -- <previous non-chord voice
                                                --  number>
accumTime :: [XMsrData] -> [(Int,XMsrData)]
accumTime = catMaybes . snd . mapAccumL f (TimeState 0 0 Nothing)
  where
    f :: TimeState -> XMsrData -> (TimeState,Maybe (Int,XMsrData))
    f (TimeState prevTime currTime pv) d = case d of
      XMDBackup d ->  (TimeState currTime (currTime-d) pv, Nothing)
      XMDForward d -> (TimeState currTime (currTime+d) pv, Nothing)
      x@(XMDNote n)
        | xnChord n -> ( TimeState prevTime currTime pv
                       , Just (prevTime, updateChordVoice pv x) )
        | otherwise -> ( TimeState currTime (currTime + xnDuration n)
                                   (xnVoice n)
                       , Just (currTime, x) )
      x -> (TimeState prevTime currTime pv, Just (currTime, x))
      

updateChordVoice :: Maybe Int -> XMsrData -> XMsrData
updateChordVoice (Just v) (XMDNote n) = case xnVoice n of
  -- exhausting cases here means a chord note was marked with a voice
  -- unexpectedly
  Nothing -> XMDNote n { xnVoice = Just v }
-- exhausting above cases mean we found a chord note but no voice was defined
-- on the previous non-chord note, or the XMsrData passed was not a note

computeMsrDatas :: Map Int IXMsrInfo -> [XMsr] -> Map Loc [XMsrData]
computeMsrDatas mis msrs 
  = UM.listToLMap . map verifyVoiceSet . concatMap f 
    $ msrs
  where
    f :: XMsr -> [(Loc,XMsrData)]
    -- okay so if we can get offset from XMsrData we are good
    f m = map (\(d,x) -> (toLoc (d + getOffset x),x)) . accumTime $ 
          XD.xmMsrDatas m
      where
        toLoc divs = case divsToLoc mis (XD.xmMsrNum m) divs of {Just x->x}


getOffset :: XMsrData -> Int
getOffset (XMDDirection _ Nothing  _ _) = 0
getOffset (XMDDirection _ (Just i) _ _) = i
getOffset _                             = 0


{-
checkIfSlurs :: (Loc,XMsrData) -> (Loc,XMsrData)
checkIfSlurs (loc, d@(XMDNote (XNNote _ _ _ _ _ _ _ _ notations _)))
  | length [x | XNSlur x _ <- notations] > 0 =
      throwMine $ "can't handle slurs - there is one at " ++ (simpleShowLoc loc)
  | otherwise = (loc,d)
checkIfSlurs x = x
-}

verifyVoiceSet :: (Loc,XMsrData) -> (Loc,XMsrData)
verifyVoiceSet t@(loc, XMDDirection xd _ v _)
  | isJust v = t
  | isNothing v = case xd of 
      XDDynamics _ -> throwMine $ printf ("XDDynamics missing voice number"++
        " at %s") (showLoc2 loc)
      XDWords _ (Just _) -> throwMine $ printf ("<words> at %s are in a " ++
        "bad state--they have a default-y attribute but no voice number")
        (showLoc2 loc)
      _ -> t
verifyVoiceSet t@(loc, XMDNote n)
  | isJust . xnVoice $ n = t
  | otherwise = throwMine $ printf ("XMsrData of constructor XMDNote" ++
                " at %s is missing a voice number") (simpleShowLoc loc)
verifyVoiceSet x = x

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

----------------------------------------------------------------------
----------------------------------------------------------------------
--          splitting staves and adjusting voices


-- 
--
splitMsrDataMap :: Map String (Map Loc [XMsrData]) -> 
                   Map String (Map Loc [XMsrData])
splitMsrDataMap = M.fromList . concatMap g . M.toList
  where
    g :: (String,Map Loc [XMsrData]) -> [(String,Map Loc [XMsrData])]
    g (name,m) = nameStaves . splitAndFixVoice $ m
      where
        nameStaves :: [(Int,Map Loc [XMsrData])] -> 
                      [(String,Map Loc [XMsrData])]
        nameStaves [(_,m)] = [(name,m)]
        nameStaves xs = map appendStaffName xs
        appendStaffName (staffNum,m) = (name ++ "-staff" ++ show staffNum, m)

-- splitAndMarkVoice 
--
-- returns [(<staff num>, <XMsrData map>)]
splitAndFixVoice :: Map Loc [XMsrData] -> [(Int,Map Loc [XMsrData])]
splitAndFixVoice msrDatas
  | length staffNums == 1 = [(S.findMin staffNums, fixVoices True msrDatas)]
  | length staffNums > 1 =
    map (\i -> (i, fixVoices False . filterStaffNum msrDatas $ i))
        (S.toList staffNums)
  where
    -- Make a set containing all unique staff numbers Just n. Staff numbers
    -- that are 'Nothing' will trigger a case exhaustion
    staffNums = foldr step S.empty . map getStaff . concat . M.elems $ msrDatas
    step (Just n) s = S.insert n s
    -- case exhaustion here means a staff number was Nothing

-- filterStaffNum
--
filterStaffNum :: Map Loc [XMsrData] -> Int -> Map Loc [XMsrData]
filterStaffNum msrDatas num = UM.lMapMaybe go msrDatas
  where
    go :: XMsrData -> Maybe XMsrData
    go d = case getStaff d of
      -- Case exhaustion here means a staff number was Nothing
      Just s | s == num  -> Just d
             | otherwise -> Nothing



-- returns true if the voice number of this XMsrData item is "essential",
-- meaning that all of the following are true:
--
--   (1) it might have been altered by Sibelius during the xml export
--
--   (2) it's either a note voice, or it's important this voice match the
--   note voices
--
--   (3) it must be set when the algorithm is finished
hasEssentialVoice :: XMsrData -> Bool
hasEssentialVoice (XMDNote XNNote {})                      = True
hasEssentialVoice (XMDDirection XDWords {} _ mVoice _)     = isJust mVoice
hasEssentialVoice (XMDDirection XDDynamics {} _ _ _)       = True
hasEssentialVoice (XMDDirection XDOtherDirection {} _ _ _) = True
-- what remains is forward, backup, "other", XNRest, XDMetronome, XDWedge,
-- XDPedal
hasEssentialVoice _                                        = False

getVoice :: XMsrData -> Maybe Int
getVoice (XMDDirection _ _ v _) = v
getVoice (XMDNote n)          = XD.xnVoice n
getVoice _                    = Nothing

getStaff :: XMsrData -> Maybe Int
getStaff (XMDDirection _ _ _ s) = s
getStaff (XMDNote n)          = XD.xnStaff n
getStaff _                    = Nothing

-- setVoice
--
-- Given an input voice V, modify the XMsrData so its voice will be Just V.
setVoice :: Int -> XMsrData -> XMsrData
setVoice v (XMDDirection x y _ s) = XMDDirection x y (Just v) s
setVoice v (XMDNote n) = XMDNote (n { XD.xnVoice = Just v })
setVoice _ x = x

-- fixVoices 
-- 
-- Possibly change some of the voice numbers so that each staff starts with 1.
--
-- If 'wasSingleStaff' is True then enforce that the existing voices
-- numbers already started at 1.
--
-- Enforce that all essential voice numbers are not Nothing.
--
-- Input XMsrData is assumed to be from a single staff only.
fixVoices :: Bool -> Map Loc [XMsrData] -> Map Loc [XMsrData]
fixVoices wasSingleStaff msrDatas = case S.minView voiceNums of
  Just (minVoice,_) | wasSingleStaff && minVoice == 1 -> msrDatas
                    | not wasSingleStaff -> 
                        M.map (map $ normalizeVoice minVoice) msrDatas
  Nothing -> throwMine $ "Problem in Process.hs:fixVoices. Maybe no notes on "++
             "one of the staves?"
  where
    voiceNums = computeEssentialVoiceNumbers msrDatas


-- Compute essential voice numbers present in the staff and throw an
-- exception if any of them are Nothing.
computeEssentialVoiceNumbers :: Map Loc [XMsrData] -> Set Int
computeEssentialVoiceNumbers = 
  foldr S.insert S.empty . mapMaybe maybeGetVoice . concat . M.elems
  where
    -- Here in maybeGetVoice we enforce all essential voices are "Just v"
    maybeGetVoice :: XMsrData -> Maybe Int
    maybeGetVoice d
      -- case exhaustion in the case statement here means some essential
      -- XMsrData didn't have its voice set
      | hasEssentialVoice d = case getVoice d of {Just v -> Just v}
      | otherwise           = Nothing


-- Given that the minimum existing essential voice number is 'minVoice', then
-- check if this is an essential voice, and if so adjust it in such as way
-- that all voices will be normalized at a minimum of 1.
normalizeVoice :: Int -> XMsrData -> XMsrData
normalizeVoice minVoice d
  | hasEssentialVoice d && currentV >= minVoice = 
      setVoice (currentV-minVoice+1) d
  | not (hasEssentialVoice d) = d
    where currentV = case getVoice d of {Just x -> x}

quickShowMsrData (XMDDirection _ _ v s) = printf "XMDDirection"
quickShowMsrData (XMDNote (XNRest {} )) = printf "XNRest"
quickShowMsrData (XMDNote (XNNote {} )) = printf "XNNote"

