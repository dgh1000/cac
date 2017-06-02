
module Instruments.ToUnitTimeMods where



import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Control.Monad.State
import Text.Printf
import Control.Monad
import Data.Either
import Data.Map(Map)
import Data.Set(Set)
import Data.Maybe
import Score.ScoreData
import Instruments.InstrumentsData
import Common.CommonData
import Common.CommonUtil
import Util.Exception
import Data.Ratio

-- we sometimes need to look up w's or other markers. on first pass pair
-- certain Marks with w's or whatever

-- it would be helpful to sort Marks first or extract

-- pair every mark with


-- things that are changed to unit time mods

-- staffAdjusts, globalAdjusts, warps, abs warps, ramps


-- how do I tweak ramps? similar

----------------------------------------------------------------------
----------------------------------------------------------------------

-- computeUnitTimeMods
--
--   From **MARKS**, compute all UNIT TIME MODS (both staff-only and global)
-- Returns (<global>, <staff only>)
--
computeUnitTimeMods :: Score -> ([UnitTimeMod], Map String [UnitTimeMod])
computeUnitTimeMods score = (globals, M.fromListWith (++) staffs)
  where
    -- Context <all staff names> <time sigs> <markers>
    context = Context (M.keys $ scStaves score) (scTimeSigs score)
                      (scMarkers score)
    oneType g = g context (scMarksByStaff score)
    xs = oneType doAdjust ++ oneType doWarps ++ oneType doAbsWarps
         ++ oneType doPauses ++ oneType doMultPauses
         ++ oneType doTwoModifies
    filt :: UnitTimeMod -> Either UnitTimeMod (String,[UnitTimeMod])
    filt m@(UnitWarp mStaffName _ _) = case mStaffName of
      Just n -> Right (n,[m])
      Nothing -> Left m
    filt m@(UnitAdjust mStaffName _ _ _ _) = case mStaffName of
      Just n -> Right (n,[m])
      Nothing -> Left m
    filt m = Left m
    (globals,staffs) = partitionEithers $ map filt xs


----------------------------------------------------------------------
----------------------------------------------------------------------

{-

doStaffAdjusts :: Context -> MarksByStaff -> [UnitTimeMod]
doStaffAdjusts (Context _ timeSigs _) marks =
    concatMap doStaff $ M.toList marksst
  where
    doStaff (staffN,marksByLoc) = concatMap doLoc $ M.toAscList marksByLoc
      where
        doLoc (loc2,marks) = concatMap doMark marks
          where
            doMark m = case m of
              StaffAdjust width amt dir ->
                let widthR = approxRational width 0.005
                    amtR   = approxRational (amt*dir)   0.005
                    loc1 = tutFromMaybe $ locAddQuar timeSigs loc2 (-widthR)
                    loc3 = tutFromMaybe $ locAddQuar timeSigs loc2   widthR
                in [UnitWarp (Just staffN) (Right (loc1,loc2,loc3)) amtR]
              _ -> []

tutFromMaybe (Just x) = x


doGlobAdjusts :: Context -> MarksByStaff -> [UnitTimeMod]
doGlobAdjusts (Context _ timeSigs _) = concatMap g . M.toList
  where
    g (staffName,m2) = concatMap h $ M.toAscList m2
      where
        h (loc2,m3) = concatMap j m3
          where
            j :: MarkD -> [UnitTimeMod]
            j (GlobAdjust width amt dir) = 
                let widthR = approxRational width 0.005
                    amtR   = approxRational (amt*dir)   0.005
                    loc1 = tutFromMaybe $ locAddQuar timeSigs loc2 (-widthR)
                    loc3 = tutFromMaybe $ locAddQuar timeSigs loc2   widthR
                in [UnitWarp Nothing (Right (loc1,loc2,loc3)) amtR]
            j _ = []
-}

----------------------------------------------------------------------
--                     Warp

-- so we convert warps to 
doWarps :: Context -> MarksByStaff -> [UnitTimeMod]
doWarps con = concatMap g . M.toAscList
  where
    g :: (String,Map Loc [MarkD]) -> [UnitTimeMod]
    g (staffName,m2) = concatMap h $ M.toAscList m2
      where
        h :: (Loc,[MarkD]) -> [UnitTimeMod]
        h (loc,m3) = concatMap j m3
          where
            j :: MarkD -> [UnitTimeMod]
            j w@Warp{} = doOneWarp con (staffName, (loc,w))
            j _        = []


doOneWarp :: Context -> ConMark -> [UnitTimeMod]
doOneWarp
  (Context _ timeSigs staffMarkers)
  (staffName, (loc,Warp leftArr rightArr mWid amt glob)) =
  case (leftLoc,rightLoc) of
    (Just loc1,Just loc3) ->
      [UnitWarp mStaffName (Right (loc1,loc,loc3)) (dir*amtR)]
    (Just locL,Nothing)   ->
      [UnitWarp mStaffName (Left (loc,locL)) ( dir*amtR)]
    (Nothing,Just locR)   ->
      [UnitWarp mStaffName (Left (loc,locR)) (-dir*amtR)]
  where
    Markers ws _ _ _ = case M.lookup staffName staffMarkers of {Just x -> x}
    -- first compute leftLoc and rightLoc
    (leftLoc,rightLoc) = case mWid of
       Nothing -> ( (if leftArr /= 0
                       then Just $ findLeftMarker loc ws "Warp"
                       else Nothing)

                  , (if rightArr /= 0
                       then Just $ findRightMarker loc ws "Warp"
                       else Nothing)
                  )
       Just w  -> ( (if leftArr /= 0
                       then Just $ computeLocSub timeSigs loc w "Warp"
                       else Nothing)
                  , (if rightArr /= 0
                       then Just $ computeLocAdd timeSigs loc w "Warp"
                       else Nothing)
                  )
    dir = computeWarpDirection leftArr rightArr loc
    amtR = approxRational amt 0.001
    mStaffName
      | not glob && leftArr*rightArr==0 = throwMine $
        printf "staff warp at %s cannot be single-sided" (showLoc2 loc)
      | not glob = Just staffName
      | glob     = Nothing



computeWarpDirection :: Int -> Int -> Loc -> Rational
computeWarpDirection x y loc
  | x == 0 && y == 0 = cwdMsg loc
  | x /= 0 && y == 0 = fromIntegral x
  | x == 0 && y /= 0 = fromIntegral y
  | x == y           = fromIntegral x
  | otherwise        = cwdMsg loc


cwdMsg loc = throwMine $ printf ("in computing warp direction at %s, " ++
             "the left and right arrows are not consistent or both zero " ++
             "as passed to computeWarpDirection") (showLoc2 loc)


computeLocAdd :: Map Int TimeSig -> Loc -> Double -> String -> Loc
computeLocAdd timeSigs loc x msg =
  case locAddQuar timeSigs loc (approxRational x 0.001) of
    Nothing -> throwMine $ printf ("While doing a Loc addition at %s, " ++
                           "was unable to complete it because locAddQuar " ++
                           "returned Nothing") (showLoc2 loc)
    Just l -> l


computeLocSub :: Map Int TimeSig -> Loc -> Double -> String -> Loc
computeLocSub timeSigs loc x msg =
  case locAddQuar timeSigs loc (approxRational (-x) 0.001) of
    Nothing -> throwMine $ printf ("While doing a Loc subtraction at %s, " ++
                           "was unable to complete it because locAddQuar " ++
                           "returned Nothing") (showLoc2 loc)
    Just l -> l


findLeftMarker :: Loc -> Set Loc -> String -> Loc
findLeftMarker loc set msg = case S.lookupLT loc set of
  Nothing -> throwMine $ printf ("at '%s', for mark of type '%s', can't " ++
             "find left marker") (showLoc2 loc) msg
  Just x  -> x


findRightMarker :: Loc -> Set Loc -> String -> Loc
findRightMarker loc set msg = case S.lookupGT loc set of
  Nothing -> throwMine $ printf ("at '%s', for mark of type '%s', can't " ++
             "find right marker") (showLoc2 loc) msg
  Just x  -> x


----------------------------------------------------------------------
----------------------------------------------------------------------

doTwoModifies :: Context -> MarksByStaff -> [UnitTimeMod]
doTwoModifies con = concatMap g . M.toAscList
  where
    g (staffName,m_staff) = concatMap h $ M.toAscList m_staff
      where
        h (loc,marks) = concatMap j marks
          where
            j t@TwoModify{} = doOneTwoModify con (staffName, (loc,t))
            j _             = []


doOneTwoModify (Context _ _ staffMarkers)
               (staffName, (loc,TwoModify tm1 tm2 mNormalize))
    = [Unit2Modify leftLoc loc rightLoc tm1 tm2 mNormalize]
  where
    Markers ws _ _ _ = case M.lookup staffName staffMarkers of {Just x -> x}
    leftLoc  = findLeftMarker  loc ws "TwoModify"
    rightLoc = findRightMarker loc ws "TwoModify"


----------------------------------------------------------------------
----------------------------------------------------------------------

doAdjust :: Context -> MarksByStaff -> [UnitTimeMod]
doAdjust con = concatMap g . M.toAscList
  where
    g (staffName,m_staff) = concatMap h $ M.toAscList m_staff
      where
        h (loc,marks) = concatMap j marks
          where
            j t@Adjust{} = doOneAdjust con (staffName, (loc,t))
            j _             = []


doOneAdjust (Context _ timeSigs staffMarkers)
               (staffName, (loc,Adjust mWid amt dir globFlag))
    = [UnitAdjust (if globFlag then Nothing else Just staffName)
                  leftLoc loc rightLoc amtR ]
  where
    Markers _ _ _ cs = case M.lookup staffName staffMarkers of {Just x -> x}
    leftLoc  = case mWid of
      Nothing -> findLeftMarker  loc cs "Adjust"
      Just w  -> computeLocSub timeSigs loc w "Adjust"
    rightLoc = case mWid of
      Nothing -> findRightMarker loc cs "Adjust"
      Just w  -> computeLocAdd timeSigs loc w "Adjust"
    amtR = approxRational (amt*dir) 0.001


----------------------------------------------------------------------
--                               AbsWarp


doAbsWarps :: Context -> MarksByStaff -> [UnitTimeMod]
doAbsWarps con = concatMap g . M.toAscList
  where
    g :: (String,Map Loc [MarkD]) -> [UnitTimeMod]
    g (staffName,marks) = concatMap h $ M.toAscList marks
      where
        h :: (Loc,[MarkD]) -> [UnitTimeMod]
        h (loc,marks2) = mapMaybe j marks2
          where
            j w = doOneAbsWarp con (staffName,(loc,w))

           

doOneAbsWarp :: Context -> ConMark -> Maybe UnitTimeMod
doOneAbsWarp (Context allStaffNames timeSigs staffMarkers)
  (staffName, (loc,AbsWarp side dur)) = Just $ UnitAbsWarp loc1 loc2 amtR
  where
    Markers ws _ _ _ = case M.lookup staffName staffMarkers of {Just x -> x}
    amtR = approxRational dur 0.001
    (loc1,loc2) = case side of
      LeftWarp  -> (loc,findLeftMarker  loc ws "AbsWarp")
      RightWarp -> (loc,findRightMarker loc ws "AbsWarp")
doOneAbsWarp _ _ = Nothing
  
  


----------------------------------------------------------------------
----------------------------------------------------------------------
--                     Ramps

{-
data Ramp2 = RampBeg2    Double
           | RampEnd2    Double
           | RampEndBeg2 Double Double
-}


{-


          APPARENTLY RAMPS ARE NOW DONE TOGETHER WITH TEMPO MARKS


-- doRamps
--
doRamps :: Context -> MarksByLoc -> [UnitTimeMod]
doRamps (Context allStaffNames timeSigs _) marks =
    mapMaybe g $ M.toAscList ramps
  where
    -- marks  :: Map Loc (Map String [MarkD])
    -- ramps :: Map Loc Ramp2
    ramps = M.mapMaybeWithKey findRamps marks
    g :: (Loc,MarkD) -> Maybe UnitTimeMod
    g (loc1,r) = case r of
        RampBeg r1  -> case mNext of
          Nothing -> msg "RampBeg was followed by no other ramp mark"
          Just (loc2,RampEndBeg r2 _) -> Just $ UnitRamp loc1 loc2 r1 r2
          Just (loc2,RampEnd    r2  ) -> Just $ UnitRamp loc1 loc2 r1 r2
          Just (_   ,RampBeg    _   ) -> msg ("RampBeg was followed " ++
                                               "by another RampBeg")
        RampEndBeg _ r1 -> case mNext of
          Nothing -> msg "RampEndBeg was followed by no other ramp mark"
          Just (loc2,RampEndBeg r2 _) -> Just $ UnitRamp loc1 loc2 r1 r2
          Just (loc2,RampEnd    r2  ) -> Just $ UnitRamp loc1 loc2 r1 r2
          Just (_   ,RampBeg    _   ) -> msg ("RampEndBeg was followed " ++
                                               "by a RampBeg")
        RampEnd _ -> Nothing
        where
          mNext = M.lookupGT loc1 ramps
          msg x = throwMine $ printf "at %s, %s" (showLoc2 loc1) x



isRampTypeMark (RampBeg _)      = True
isRampTypeMark (RampEndBeg _ _) = True
isRampTypeMark (RampEnd _)      = True



findRamps :: Loc -> Map String [MarkD] -> Maybe MarkD
findRamps loc = findRamps_2 loc . concat . M.elems


findRamps_2 :: Loc -> [MarkD] -> Maybe MarkD
findRamps_2 loc =  g . filter isRampTypeMark
  where
    g :: [MarkD] -> Maybe MarkD
    g [] = Nothing
    g ms =
      let begs    = [r | r@RampBeg   {} <- ms]
          ends    = [r | r@RampEnd   {} <- ms]
          endBegs = [r | r@RampEndBeg{} <- ms]
      in case (begs,ends,endBegs) of
           ([] ,[] ,[])  -> Nothing
           ([x],[] ,[])  -> Just x
           ([] ,[x],[])  -> Just x
           ([] ,[] ,[x]) -> Just x
           _ -> throwMine $ printf ("at %s, found a combination of " ++
                "ramps that is not allowed; there must be only " ++
                "one RampBeg, RampEnd, or RampEndBeg") (showLoc2 loc)


-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                   Pause

doPauses :: Context -> MarksByStaff -> [UnitTimeMod]
doPauses _ = concatMap h . M.elems
  where
    -- how does this change to change to Map String (Map Loc [MarkD])?
    -- a pause on any staff
    h :: Map Loc [MarkD] -> [UnitTimeMod]
    h = concatMap j . M.toList
    j :: (Loc,[MarkD]) -> [UnitTimeMod]
    j (loc,ms) = mapMaybe (maybePause loc) ms

    
maybePause :: Loc -> MarkD -> Maybe UnitTimeMod
maybePause loc (Pause (Right v)) =
  Just $ UnitPause loc $ approxRational v 0.001
maybePause _ (Pause _) = error "didn't implement Pause of cons Left"
maybePause _ _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--                   MultPause

doMultPauses :: Context -> MarksByStaff -> [UnitTimeMod]
doMultPauses (Context _ timeSigs _) = concatMap g . M.toList
  where
    -- marks :: Map Loc (Map String [MarkD])
    g :: (String,Map Loc [MarkD]) -> [UnitTimeMod]
    g (_,ms) = concatMap h $ M.toAscList ms
    h :: (Loc,[MarkD]) -> [UnitTimeMod]
    h (loc,ms) = concatMap (maybeMultPause timeSigs loc) ms


-- MultPause <num pauses> <separation in quarters>
--           <largest duration in quarters>
maybeMultPause :: Map Int TimeSig -> Loc -> MarkD -> [UnitTimeMod]
maybeMultPause ts loc (MultPause n sep dur) = map f [1..n]
  where
    sepR = approxRational sep 0.001
    durR = approxRational dur 0.001
    smallestPause = dur/fromIntegral n
    f :: Int -> UnitTimeMod
    f i = UnitPause locOut (fromIntegral (n-i+1)*durR/fromIntegral n) 
      where
        locOut = case locAddQuar ts loc (fromIntegral (1-i)*sepR) of
          Just x -> x
maybeMultPause _ _ _ = []


----------------------------------------------------------------------
----------------------------------------------------------------------
--                            RitPause

{-

doRitPauses :: Context -> Map String (Map Loc [MarkD]) -> [UnitTimeMod]
doRitPauses (Context _ timeSigs _) = concatMap g . M.toList
  where
    g :: (String,Map Loc [MarkD]) -> [UnitTimeMod]
    g (_,m) = concatMap h $ M.toList m
    h (loc,ms) = concatMap (maybeRitPause timeSigs loc) ms


maybeRitPause :: Map Int TimeSig -> Loc -> MarkD -> [UnitTimeMod]
maybeRitPause ts loc (RitPause ratio1 ratio2 width amt) =
    [UnitPause loc amtR,UnitRamp locL loc 1 ratio1,UnitRamp loc locR ratio2 1]
  where
    -- ratio1 = computeNumVar vars loc "RitPause" ratio1NV
    -- ratio2 = computeNumVar vars loc "RitPause" ratio2NV
    widthR  = approxRational width 0.001
    amtR    = approxRational amt   0.001
    locL = case locAddQuar ts loc (-widthR) of {Just x -> x}
    locR = case locAddQuar ts loc   widthR  of {Just x -> x}
maybeRitPause _ _ _ = []

-}



  
                            
                          
       
