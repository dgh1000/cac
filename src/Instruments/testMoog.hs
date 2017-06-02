
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.List as L
import qualified Data.Map as M
import Text.Printf
import Data.Map(Map)
import Common.CommonData
import Common.CommonUtil
import Util.Exception
import Util.Showable

type StSectionLayout = Map Loc (Loc,Int) -- <beg loc> (<end loc>,<track num>)

type TrkSectionLayout = Map Loc (Loc,String) -- <beg loc> (<end loc>, <artic>)

data LayoutState = LayoutState (Map String StSectionLayout)
                               (Map Int TrkSectionLayout)


instance ShowItemClass LayoutState where
  showI (LayoutState m1 m2) =
    Component "LayoutState" True [showI $ StSect m1, showI $ TrkSect m2]

data StSect = StSect (Map String StSectionLayout)

instance ShowItemClass StSect where
  showI (StSect m) =
    Component "Staff sections" True (map (showI . SSL) $ M.toAscList m)


data SSL = SSL (String,Map Loc (Loc,Int))

instance ShowItemClass SSL where
  showI (SSL (stName,m)) = Component stName True (map g $ M.toAscList m)
    where
      g (loc1,(loc2,trNum)) =
        SingleLine $ printf "%s -> %s, trk:%d" (showLoc2 loc1) (showLoc2 loc2)
                     trNum

data TrkSect = TrkSect (Map Int TrkSectionLayout)

instance ShowItemClass TrkSect where
  showI (TrkSect m) =
    Component "Track sections" True (map (showI . TSL) $ M.toAscList m)


data TSL = TSL (Int,Map Loc (Loc,String))

instance ShowItemClass TSL where
  showI (TSL (trackNum,layout)) = Component (show trackNum) True
                                            (map g $ M.toAscList layout)
    where
      g (loc1,(loc2,a)) =
        SingleLine $ printf "%s -> %s, artic:%s" (showLoc2 loc1)
                     (showLoc2 loc2) a



x1 = M.fromList [ (Loc 1 1,"articNikki")
                , (Loc 1 3,"articNodin") ]

last1 = Loc 2 1
  
x2 = M.fromList [ (Loc 1 2,"articBob")
                , (Loc 1 4,"articNodin") ]

last2 = Loc 2 3


-- okay so we need to use a portion of track if it already is usuable. that is, when we request 

m1 = M.fromList [ ("staff1",x1)
                , ("staff2",x2) ]

lastLocs = [("staff1",last1),("staff2",last2)]

main = 
  putStrLn . showIString $ computeLS lastLocs 2 m1
        

computeLS :: [(String,Loc)] -> Int -> Map String (Map Loc String) ->
             LayoutState
computeLS lastLocs numTracks artics = foldr (doStaff lastLocs) ils $
                                      M.toList artics
  where
    ils =
      LayoutState (M.map (const M.empty) artics)
                  (M.fromList $ map (\i -> (i,M.empty)) [0..numTracks-1])
                                                       
    

doStaff :: [(String,Loc)] -> (String,Map Loc String) -> LayoutState ->
           LayoutState
doStaff lastLocs (staffN,m) layoutState =
  foldr (addStaffSection staffN) layoutState ys
  where
    ll = mgLookupL staffN lastLocs
    xs = M.toAscList m ++ [(ll,"")]
    ys :: [(Loc,(Loc,String))]
    ys = map f $ zip xs (drop 1 xs)
    f ((loc1,artic),(loc2,_)) = (loc1,(loc2,artic))




-- addStaffSection
--
--   Given a staff section from loc1 to loc2, with artic 'artic', update the
--   LayoutState as follows:
--
--     - find a track that has a track section overlapping with loc1 to loc2,
--       and that track section is already dedicated to 'artic': call first
--       one found T1
--
--     - find a track that is totally free within loc1 to loc2. call first one
--       found T2
--
--     - if T1 exists, then choose X=T1. otherwise if T2 exists, choose
--       X=T2. otherwise give error
--

addStaffSection ::  String -> (Loc,(Loc,String)) -> LayoutState ->
                    LayoutState
addStaffSection
  staffN (loc1,(loc2,artic)) (LayoutState stSections trkSections)
  = (LayoutState newStSections newTrkSections)
  where
    (idxFree,layFree) = case L.find (isFree . snd) $ M.toList trkSections of
      Nothing -> throwMine "kdj^"
      Just x -> x
    isFree :: TrkSectionLayout -> Bool
    isFree = all g . M.toAscList
      where
        g (locA,(locB,_)) = loc2 <= locA || loc1 >= locB

    newTrkSections = M.insert idxFree (M.insert loc1 (loc2,artic) layFree)
                     trkSections
    -- so we need to initialize staff layout
    existingStLay = mgLookup staffN stSections
    newStLay = M.insert loc1 (loc2,idxFree) existingStLay
    newStSections = M.insert staffN newStLay stSections

{-

-- addStaffSection_help1
--
--   Given a staff section, (loc1, loc2), find a track that is entirely
--   unused between loc1 and loc2
addStaffSection_help1 :: Loc -> Loc -> Map Int TrkSectionLayout ->
                         Maybe Int
addStaffSection_help1 loc1 loc2 trkSections =
    fmap fst $ find (isFree . snd) $ M.toAscList trkSections
  where
    isFree :: TrkSectionLayout -> Bool
    isFree = all g . M.toAscList
      where
        g (locA,(locB,_)) = loc2 <= locA || loc1 >= locB


-- addStaffSection_help2
--
--   Given a staff section (loc1, loc2, artic), find a track that is not
--   entirely free, but (loc1 -> loc2) overlaps with a used section of the
--   same artic.
--
--   We need to look at each existing track section and put into one of three
--   categories:
--
--     Nothing: no overlap
--     Just True: overlaps, and artic matches
--     Just False: overlaps and artic doesn't match
--
--   If there are no "Just False's" and at least one Just True, then the track
--   matches.
--
--
--       locA              locB         <-- a particular track section
--    loc1      loc2
--       loc1    loc2
--          loc1  loc2
--   loc1             loc2
-- loc1  loc2
--           loc1     loc2
--           
--

--
addStaffSection_help2 :: Loc -> Loc -> String -> Map Int TrkSectionLayout ->
                         Maybe Int
addStaffSection_help2 loc1 loc2 articIn trkSections =
  where
    M.toAscList trkSections
    -- TrkSectionLayout is Map Loc (Loc,String)
    -- [(Loc,(Loc,String)]
    g :: TrkSectionLayout -> Bool
    g t = case xs of
      [] -> False
      ys -> all id ys
      where
        xs = mapMaybe sameArtic $ M.toAscList t

    sameArtic :: (Loc,(Loc,String)) -> Maybe Bool
    sameArtic (locA,(locB,art))
      | overlaps  = Just $ art == articIn
      | otherwise = Nothing
      where
        ovlp1 = loc1 >= locA && loc1 <  locB
        ovlp2 = loc2  > locA && loc2 <= locB
        ovlp3 = loc1 <  locA && loc2 >= locB
        overlaps = ovlp1 || ovlp2 || ovlp3

    
    
-- addStaffSection_help2
--
--   Given a staff section, (loc1, loc2, artic), find a track that overlaps
--   with some track section, and that track section contains only the same
--   artic as 'artic'
addStaffSection_help1 :: Loc -> Loc -> String -> Map Int TrkSectionLayout ->
                         Maybe Int
addStaffSection_help1 loc1 loc2 artic trkSections =
    fmap fst $ find (isFree . snd) $ M.toAscList trkSections
  where
    isFree :: TrkSectionLayout -> Bool
    isFree = all g . M.toAscList
      where
        g (locA,(locB,_)) = loc2 <= locA || loc1 >= locB
    
-}    

    
mgLookup :: Ord k => k -> Map k a -> a
mgLookup k m = case M.lookup k m of {Just x -> x}

mgLookupL k m = case lookup k m of {Just x -> x}
