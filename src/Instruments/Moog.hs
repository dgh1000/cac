
module Instruments.Moog where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Data.Map(Map)
import Control.Monad
import Control.Monad.State
import Common.CommonData
import Text.Printf
import Instruments.Curves
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Instruments.TimeMap
import Util.Exception
import Instruments.AlterTimes
import Score.ScoreData


-- need to know staff names, for each staff there are two Moog instr. moog
-- instr hold mainly stream/chan.
--
-- curves
--
--   should curves be across entire composition, or just normalized to 0 to 1?
--   well I'm assuming that we mark a section in the composition, a segment,
--   with a name such as "c". in the configuration for this composition, we
--   define a curve "c". we aren't going to put locations in the config. we
--   don't want to assume a certain duration... or do we? duration in beats?
--   certainly not in seconds as that could change with changing tempo. I
--   think it's easiest to assume the curve will be defined to run from 0 to 1
--   and that will be mapped onto the composition every segment marked
--   "c". that way we can reuse curve definitions, and general curves will be
--   handy.

makeMoog :: String -> [(String,Int)] -> Meta
makeMoog name dests
  = MetaMoog Moog
    { mgInit = moogInit_impl
    , mgRun  = moogRun_impl
    , mgCommon = MetaCommon { mcName        = name
                            , mcStaffNs     = map fst dests
                            , mcTremShapes  = M.empty
                            , mcTrillShapes = M.empty
                            , mcStacDurs    = M.empty
                            , mcArtics      = M.empty
                            , mcArpDelta    = M.empty
                            , mcLegExt      = M.empty
                            , mcTrunc       = M.empty
                            , mcCurves      = M.empty
                            }
    , mgTrks          = map snd dests
    , mgCtrlNums      = [("freqFilt1", (1,102))]
    , mgArtics        = [( "fooArtic", MoogArtic "fooPatch" []
                                       (VelCurve [(0.45,10),(8.55,127)])
                         )]
    , mgBuildCurves   = buildCurves
    }
                     

 

-- moogInit_impl
--
-- This has to
--
--   - compute LayoutState for the score; that is look at each staff, and each
--     artic mark on those staves, and figure out what each section of a staff
--     (that has the same artic) will be mapped to a track
--
moogInit_impl :: Moog -> Tr Moog
moogInit_impl moog = do
  staves <- scStaves `liftM` gets tsScore
  let lastLocs = map (stName &&& stMaxTrueEnd) $ M.elems staves
      ls = computeLS lastLocs (mgNumTrks moog) (mcArtics $ mgCommon moog)
  let -- compute curves. curves will be identified in the 
  return moog {mgLayout = ls}

{-

-- computeLayouts
--
--   Determine how each track will be "laid out" - that is, what artic will go
--   over what time intervals on each track. Call a section of track spanning
--   (loc1,loc2) a "track section". Call a section of a staff (S1,S2) in which
--   there is one artic a "staff section". We need to map each staff section
--   to a track section.
computeLayouts :: Moog -> Map String (Map Loc (Loc,String)) ->
                  Tr (Map Int TrackLayout)
computeLayouts moog staffArtics = do
  let findMatchingTrackNums patchName
          | null out  = throwMine "j26has"
          | otherwise = out
        where
          out = map fst $ filter (g . snd) $ zip [0..] (mgTracks moog)
          g (MoogTrack _ pn) = pn == patchName
      step :: Map Int TrackLayout -> ((Loc,Loc),String) -> Map Int TrackLayout
      step trackLayouts ((loc1,loc2),articName) =
          M.insert freeTrackNum updatedTrack trackLayouts
        where
          (MoogArtic patchN _ _) = mgLookupL articName (mgArtics moog)
          matchingTrackNums = findMatchingTracksNums patchN
          -- now we need to find one that's free
          freeTrackNum = case L.find checkFree matchingTrackNums of
            Nothing -> throwMine "dvb35"
            Just n  -> n
          updatedTrack = M.insert loc1 (loc2,articName)
                         $ mgLookup freeTrackNum trackLayouts
          checkFree :: Int -> Bool
          checkFree idx = all notInThisTrack $ M.toList l
            where
              l = trackLayouts !! idx
              notInThisTrack :: (Loc,(Loc,String)) -> Bool
              notInThisTrack (l1,(l2,_)) = loc1 >= l2 || loc2 <= l1
            
          
-}          



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
        
    
---------------------------------------------------------------------------
---------------------------------------------------------------------------
--                     moogRun_impl and associated functions

-- moogRun_impl
--
--   Need to
--
--     Generate midi events corresponding to notes: note on and note off, for
--     all staves.
--
--     Generate midi events corresponding to curves
--
--     Generate midi events needed to set up initial artic and changes in
--     artic throughout each staff
--     
moogRun_impl :: Moog -> Int -> Int -> Tr ()
moogRun_impl moog begMsr endMsr = do
  concat `liftM` mapM (runStaff moog begMsr endMsr) (mStaffNs $ MetaMoog moog)
    >>= includeNotes
  articControlSetup moog begMsr endMsr >> includeRaws


-- doStaff
--
--   FOR ONE STAFF, compute
--
--     the NOTE ON/OFF events
--
--     the CONTROL events corresponding to curves
--
--     the CONTROL events corresponding to setting up a patch to have a
--     particular articulation
--
runStaff moog begMsr endMsr staffName = do
  (concat `liftM` forOneStaff nominalPitsTs (MetaMoog moog)
                  staffName begMsr endMsr)
    >>= mapM (runMoogDetails moog)
    >>= includeNotes
  applyAllCurvesStaff moog staffname begMsr endMsr >>= includeRaws

-- timing that needs to be investigated
--
-- 'leadTimeArtic': how soon does an artic need to be set up via its control
--                  values before actual notes occur?
--
leadTimeArtic = 0.01



-- articControlSetup
--
-- 
articControlSetup :: Moog -> Int -> Int -> Tr [TrRaw]
articControlSetup moog msrBeg msrEnd = do
  -- find all portions of layout for this staff that fall between 'msrBeg' and
  -- 'msrEnd' and set up control events for each one
  -- we are looking at track layout
  let LayoutState staffLayout trkLayout = mgLayout moog

      doTrack :: (Int,TrkSectionLayout) -> Tr [TrRaw]
      doTrack (trkNum,tsLayout) = do
        let stream = mgIndex trkNum (mgTrks moog)

            -- doArtic takes a beg loc and an artic name, and generates all
            -- necessary midi control events
            doArtic :: (Loc,(Loc,String)) -> Tr [TrRaw]
            doArtic (beg,(_,articName)) = do
              let staffName = forTrackFindStaff (mgLayout moog) beg trkNum
              t <- lookupTimeTr beg staffName
              let MoogArtic _ ctrlValues _ = mgLookup articName $ mgArtics moog
              mapM (oneCtrl $ t-leadTimeArtic) ctrlValues
              
            --  oneCtrl takes a time, track number, ctrl name and value, and
            --  constructs one TrRaw
            oneCtrl :: Double -> Int -> (String,Int) -> Tr TrRaw
            oneCtrl time (ctrlName,value) = do
              let (chan,ctrlMidiNum) = mgLookup ctrlName $ mgCtrlNums moog
              return $ TrRaw "" time (stream,chan) 0xB0 ctrlMidiNum value
        concat `liftM` mapM doArtic (M.toList tsLayout)
  concat `liftM` mapM doTrack (M.toList trkLayout)


forTrackFindStaff :: LayoutState -> Loc -> Int -> String
forTrackFindStaff (LayoutState staffLayout trkLayout) loc trkNum =
    case L.find ((==trkNum) . snd) . map staffToTrk $ M.keys staffLayout of
      Just (name,_) -> name
  where
    -- staffLayout :: Map String StSectionLayout
    -- StSectionLayout:: Map Loc (Loc,Int)
    staffToTrk :: String -> (String,Int)
    staffToTrk staffName = case M.lookup staffName staffLayout of
      Just s -> case M.lookupLE loc s of
        (_,n) -> (staffName,n)

                     
-- applyAllCurvesStaff
--
--   Apply all curves for control points, FOR ONE STAFF.
--
applyAllCurvesStaff :: Moog -> String -> Loc -> Loc -> Tr [TrRaw]
applyAllCurvesStaff moog staffName msrBeg msrEnd =
  case M.lookup staffName . mcCurves $ mgCommon moog of
    Nothing -> return []
    Just curves -> do
      let f (curveName,(ctrlName,curve)) =
            applyCurveStaff moog staffName msrBeg msrEnd curveName curve
              ctrlName
      concat `liftM` mapM f (M.toList curves)
            
  

-- applyCurveStaff
--
--   Given a generic curve and a name 'curveName', generate TrRaw for every
--   instance of brackets with that name
--
applyCurveStaff :: Moog -> String -> Loc -> Loc -> String -> Curve ->
                   String -> Tr [TrRaw]
applyCurveStaff moog staffName msrBeg msrEnd curveName curve ctrlName = do
  -- bs :: Map String [(Loc,Loc)]
  bs <- (stBrackets . mgMapLookup staffName . scStaves) `liftM`
        gets tsScore
  let locs = case M.lookup curveName bs of
               Nothing -> []
               Just xs -> xs
      ctrlName = mgLookup curveName mgCurveAssign
      f (l1,l2) = applyCurve1 moog staffName msrBeg msrEnd l1 l2 ctrlName
                  curve
  concat `liftM` mapM f locs


-- applyCurve1
--
--   The idea is that we have some named curves, called "generic curve." An
--   example is a curve named "Nodin" that is a gentle arc starting at zero,
--   peaking at 127, and returning to zero. The domain of a generic curve will
--   always be the interval on the x-axis [0,1].
--
--   A generic curve is then "applied" to places in the composition that are
--   sepcified by bracket marks. A pair of bracket marks has a name, such as
--   "Nodin", and spans (loc1,loc2) on a particular staff. A name such as
--   "Nodin" may appear in more than one bracket mark on a staff, or appear at
--   least once on multiple staves. We'll call the appearance of a bracket
--   mark with a particular name X an "instance" of X.
--
--   The goal of this function is to apply a particular generic curve, input
--   parameter 'curve', to the staff 'staffName' between the locations 'brBeg'
--   and 'brEnd'.
--
--   The following algorithm is needed:
--
--     - determine the stream number associated with this place in the
--       staff. The staff layout will already be determined, so it is possible
--       to look up the track number associated with this region of the
--       staff. This region must be laid out on a single track, continuously,
--       or else this routine will throw an exception.
--
--       - this starts by finding the overlap of the conversion range,
--         'msrBeg' to 'msrEnd', with the bracket range.
--
--     - using input parameter 'ctrlName' look up the MIDI channel and
--       controller number associated with that control.
--
--     - compute the begin and end time, in seconds, associated with 'brBeg'
--       and 'brEnd', and create a sequence of times that are 0.02 seconds
--       apart
--
--     - at each time, map that time to a time X within the normalized generic
--       curve (that is, scale to some point between 0 and 1), query the value
--       of the curve at X, round it to an integer, and produce a control
--       point
--
applyCurve1 :: Moog -> String -> Loc -> Loc -> Loc -> Loc -> String ->
                Curve -> Tr [TrRaw]
applyCurve1 moog staffName msrBeg msrEnd brBeg brEnd ctrlName curve = do
  -- compute "conversion range" (l1,l2) (the range of locations for which we
  -- are producing output notes)
  let l1 = Loc msrBeg 1
      l2 = Loc msrEnd 1
  if brEnd <= l1 || brBeg >= l2
     then return []
     else do
       -- compute (a1,a2) which is portion of (brBeg,brEnd) which falls inside
       -- the conversion range (l1,l2)
       let a1 = max brBeg l1
           a2 = min brEnd l2
       -- find range of times matching (a1,a2) : call this (t1,t2)
       t1 <- lookupTimeTr staffName a1
       t2 <- lookupTimeTr staffName a2
       -- find destination: stream and channel at this point
       let LayoutState staffSects _ = mgLayout moog
           trkNum = case M.lookupLE a1 $ mgLookup staffName staffSects of
                 Just (loc1,(loc2,trk))
                   | loc1 <= a1 && loc2 >= a2 -> trk
                   | otherwise                -> throwMine "ka6x930"
                 Nothing -> throwMine "mn*238"
           streamNum | length $ mgTrks moog > trkNum = mgTrks moog !! trkNum
                     | otherwise = throwMine ",m,m[]"
           (chan,ctrlNum) = mgLookup ctrlName $ mgCtrlNums moog
       let g :: Double -> TrRaw
           g t = TrRaw staffName t (stream,chan) 0xB0 ctrlNum x2
             where
               tx = scaleClip t1 t t2 0 1
               x1 = case round $ curveLookup tx curve of
                 Just y -> y
               x2 = clip x1 0 127
       let times = [t1,t1+0.02..t2]
       return $ map g times
      
       


computeCtrlPoints :: Moog -> Int -> Int -> Tr [TrRaw]
computeCtrlPoints moog begMsr endMsr = do
  score <- gets tsScore
  atms  <- gets tsTimeMaps
  let staffN = case mStaffNs (MetaMoog moog) of
        (x:_) -> x
      atm = case M.lookup staffN atms of
        Just x -> x
      t1 = lookupTime (Loc begMsr 1) atm
      t2 = lookupTime (Loc endMsr 1) atm
      cs :: Map String Curve -- where key is br. segment name
      cs = mgBuildCurves moog score
      -- it appears that we assume curves are spanning the 
      doTrack :: Int -> [TrRaw] 
      doTrack stream = concatMap doCurve $ M.toList cs
        where
          doCurve (ctrlName,curve) = mapMaybe doTime [t1,t1+0.02..t2]
            where
              (chan,ctrl) = case lookup ctrlName $ mgCtrlNums moog of
                Nothing -> throwMine "93xz8bn"
                Just x  -> x
              doTime t = case curveLookup (t-0.02) curve of
                  Nothing -> Nothing
                  Just x  ->
                    let ctrlValue = round x
                    in Just $ TrRaw "moog" t (stream,chan) 0xB0 ctrl ctrlValue
  return $ concatMap doTrack $ mgTrks moog
                  
--


{-

runStaff :: Moog -> Int -> Int -> String -> Tr [TrNote]
runStaff moog begMsr endMsr staffN =
  (concat `liftM`
    forOneStaff nominalPitsTs (MetaMoog moog) staffN begMsr endMsr)
    >>= mapM (runMoogDetails moog)
    >>= alterTOff
-}

-- runMoogDetails
--
--   Given a note, 
--
runMoogDetails :: Moog -> TrNote -> Tr TrNote
runMoogDetails moog note = do
  -- time alter (staccato), destination
  slurs <- (stSlurs . mgMapLookup (tnStaffName note) . scStaves) `liftM`
           gets tsScore
  let artic  = "foo"
      (patchName,velCurve) = case mgLookup artic $ mgArtics moog of
        MoogArtic n _ c -> (n,c)
      stream = case L.find (\(MoogTrack _ p) -> p == patchName)
                      $ mgTracks moog of
        Just (MoogTrack s _) -> s
      chan = 1
      alterEnd = utilEndAlter (MetaMoog moog) slurs note
  loud <- (clipLoud . computeAccent note) `liftM` lookupLoud note
  let vel = lookupVel "k3000" (tnLoud note) velCurve
      d1 = DestData (stream,chan) (tnNomPitch note) vel []
  return note { tnDests    = [d1]
              , tnLoud     = loud
              , tnAlterEnd = alterEnd
              , tnSepSame  = 0.05
              }


mgLookup x ys = case lookup x ys of {Just z -> z}

mgMapLookup k m = case M.lookup k m of {Just x -> x}

{-
-- this was when we kept
  
lookupInstrForStaff staffN moog = case find ((staffName `elem`) . snd) $
    map (mgiStaffNs &&& id) $ map snd (mgInstrs moog) of
  Nothing -> throwMine "89167"
  Just (_,x) -> x
-}

lookupCtrlNum name moog = case lookup name (mgCtrlNums moog) of
  Nothing -> throwMine $ printf "error: no control named '%s' in Moog" name
  Just n  -> n
  
