
module Translation.ParseCmd where

import qualified Data.Map as M
import qualified Data.List as L
import Text.Printf
import Debug.Trace
import Control.Monad
import Data.Maybe
import Data.Map(Map)
import Text.Parsec
import Text.Parsec.String
import Util.Exception
import Util.Map(listToLMap, lMapToList)
import Common.CommonExport
import Score.ScoreData
import Translation.TranslationData


data ParsedCmd = Pause Int
               | Warp Bool Bool Bool Int  -- <staff flag> <is left warp>
                                          -- <is right warp> <amt>
               | Stac Int
               | Arp Int
               | Marker Char



computeTextCmdsAllStaves :: Score -> 
    (Map String (Map Loc [TextCmd]),Map Loc [TextCmd])
computeTextCmdsAllStaves score = 
    ( M.map (M.map (filter isStaffOnly)) prelimStaves
    , M.map (filter (not . isStaffOnly)) prelimGlobal
    )
  where 
    prelimStaves = M.map (computeTextCmds . stText) . scStaves $ score
    prelimGlobal = M.unionsWith (++) . M.elems $ prelimStaves


isStaffOnly :: TextCmd -> Bool
isStaffOnly (PauseCmd _)            = False
isStaffOnly (WarpCmd isStaff _ _ _) = isStaff
isStaffOnly (ArpCmd _)              = False
isStaffOnly (StacCmd _)             = True
isStaffOnly (MarkerCmd _)           = False


computeTextCmds :: Map Loc [Text] -> Map Loc [TextCmd]
computeTextCmds text = listToLMap . mapMaybe (maybeMakeCmd text) $
  [(loc,s) | (loc,TechniqueText s) <- lMapToList text]
  

maybeMakeCmd :: Map Loc [Text] -> (Loc,String) -> Maybe (Loc,TextCmd)
maybeMakeCmd text (loc,s) = case parse (parseCmd loc) "" s of
  Left _ -> Nothing 
  Right (Pause i)      -> Just (loc, PauseCmd $ fromIntegral i / 100)
  Right (Warp s l r i) -> Just (loc, computeWarp text loc s l r i)
  Right (Stac i)       -> Just (loc, StacCmd $ fromIntegral i / 100)
  Right (Arp i)        -> Just (loc, ArpCmd $ fromIntegral i / 100)
  Right (Marker c)     -> Just (loc, MarkerCmd c)


computeWarp :: Map Loc [Text] -> Loc -> Bool -> Bool -> Bool -> Int -> TextCmd
computeWarp text loc isStaff leftFlag rightFlag amt =
    WarpCmd isStaff leftLoc rightLoc (fromIntegral amt / 100)
  where
    (leftText,rightText) = M.split loc text
    warning = printf "Warning: x is quite far from warp command at %s"
              (simpleShowLoc loc)
    leftLoc = if leftFlag
      then case L.find (\(_,ts) -> hasX ts) $ M.toDescList leftText of
             Just (l,_) | xSeparationWarning l loc -> warning `trace` Just l
                        | otherwise -> Just l
             Nothing -> throwMine $ printf ("No left x found at warp cmd " ++
                        "at loc %s") (simpleShowLoc loc)
      else Nothing
    rightLoc = if rightFlag
      then case L.find (\(_,ts) -> hasX ts) $ M.toAscList rightText of
             Just (l,_) | xSeparationWarning loc l -> warning `trace` Just l
                        | otherwise -> Just l
             Nothing -> throwMine $ printf ("No right x found at warp cmd " ++
                        " at %s") (simpleShowLoc loc)
      else Nothing


xSeparationWarning :: Loc -> Loc -> Bool
xSeparationWarning (Loc msr1 _) (Loc msr2 _) = msr1 <= msr2 - 2


hasX :: [Text] -> Bool
hasX ts = any (== "x") [s | TechniqueText s <- ts]


parseCmd :: Loc -> Parser ParsedCmd
parseCmd loc = 
  try (parsePause loc) 
  <|> 
  try (parseWarp loc)
  <|>
  try (parseArp loc)
  <|>
  try (parseStac loc)
  <|>
  try (parseMarker loc)


parseInt :: Loc -> Parser Int
parseInt loc = do
  s <- many1 (oneOf "-0123456789")
  let i = case reads s of
        [] -> throwMine $ printf ("in command technique text at %s, " ++
              "malformed integer '%s'") (simpleShowLoc loc) s
        (x,_):_ -> x
  return i


parseMarker :: Loc -> Parser ParsedCmd
parseMarker loc = do
  c <- oneOf ['a'..'w']
  eof
  return $ Marker c


parseArp loc = do
  string "arp"
  Arp <$> parseInt loc


parseStac loc = do
  string "stac"
  Stac <$> parseInt loc


parsePause loc = do
  string "ps"
  Pause <$> parseInt loc


parseWarp loc = do
  left    <- isJust `liftM` optionMaybe (char '/')
  isStaff <- isJust `liftM` optionMaybe (char 's')
  char 'w'
  i       <- parseInt loc
  right   <- isJust `liftM` optionMaybe (char '/')
  eof
  when (not left && not right) (throwMine $ printf ("warp at %s must have" ++
       " at least one side") (simpleShowLoc loc))
  when (isStaff && not (left && right)) (throwMine $ printf ("staff warp " ++
       "at %s must be double-sided") (simpleShowLoc loc))
  return $ Warp isStaff left right i




{-

How do we get instrumental technique text to instruments? they must receive
it *before* translation is finished. right now in mp.hs we construct the
Instrument data by calling the associated function with Score and
StaffConfig. We construct all instruments *before* calling translation. well
we could parse text in Score. how is processing allocated between Score and
Translation anyway? I think my goal was to put as little as possible about
the instruments in Score, as little as possible about translation
specifics. what am I trying to achieve? what independence from? If I switched
to different score editor, there would be less to recode. not *that* much
less, plus there is some benefit to abstracting markings from a knowledge of
whether they are text or metronome or whatever. Could change symbol I use or
different type of symbol to mean same thing.


marks


  

  what is NOT text in XML representation

    metronome markings. they are directions. anything other than an integer
    CANNOT be accomodated, so "5:4" is right out. 


  metronome representation

    will use both standard metronome marks which appear in the XML as
    <metronome>, and technique text.

      standard can specify straight tempos.

      technique t. can specify straight tempos, percentage changes, and ratios

        if you put "<quarter symbol>=5:4" in the score, in the XML it reads
        as "=5:4"

  types of marks

    instrument technique

      will need to know what instruments might recognize and parse it
      explicitly. 

    right now text that is unrecognized will be ignored. but this means it's
    impossible to catch syntax mistakes.

      in old plan, instrumental technique is ignored by Translation.hs and
      only noticed by instruments. To catch syntax mistakes,
      probably best setup is to code any technique that any instrument might
      use in TranslationData.hs and parse it explicitly

    commands. generalized idea of text that specifies some kind of processing
    to be done. 

       examples: modify time, set arp delta, set staccato duration, mark
       splice point

    does text apply to staff or all staves?  (i.e. staff or global)

       some existing "commands" have only one possible meaning. but the
       "warp" can be either: an 's' in the text means staff-only


       do we want to create a consistent text syntax so that all staff text
       always looks a certain way, and all global a certain way? this will
       lengthen text, maybe force unnatural looking syntax. I say no.

    technique text

  do we need to have categories of 

    



-}




{-

data ParsedCmd = Pause Int
               | Warp Bool Bool Int
               | Stac Int
               | Arp Int



extractTextCmds :: Map Loc [Text] -> Map Loc [TextCmd]
extractTextCmds text = listToLMap . mapMaybe (maybeMakeCmd text) $
  [(loc,s) | (loc,TechniqueText s) <- lMapToList text]
  

maybeMakeCmd :: Map Loc [Text] -> (Loc,String) -> Maybe (Loc,TextCmd)
maybeMakeCmd text (loc,s) = case parse parseCmd "" s of
  Left _ -> Nothing 
  Right (Pause i)    -> Just (loc, PauseCmd $ fromIntegral i / 100)
  Right (Warp l r i) -> Just (loc, computeWarp text loc l r i)
  Right (Stac i)     -> Just (loc, StacCmd $ fromIntegral i / 100)
  Right (Arp i)      -> Just (loc, ArpCmd $ fromIntegral i / 100)


computeWarp :: Map Loc [Text] -> Loc -> Bool -> Bool -> Int -> Textcmd
computeWarp text loc leftFlag rightFlag amt =
    WarpCmd leftLoc rightLoc (fromIntegral amt / 100)
  where
    (leftText,rightText) = M.split loc text
    warning = printf "Warning: x is quite far from warp command at %s"
              (simpleShowLoc loc)
    leftLoc = if leftFlag
      then case L.find hasX $ M.toDescList leftText of
             Just (l,_) | xSeparationWarning l loc = warning `trace` Just l
                        | otherwise = Just l
             Nothing -> throwMine $ printf ("No left x found at warp cmd " ++
                        "at loc %s") (simpleShowLoc loc)
      else Nothing
    rightLoc = if rightFlag
      then case L.find hasX $ M.toAscList rightText of
             Just (l,_) | xSeparationWarning loc l = warning `trace` Just l
                        | otherwise = Just l
             Nothing -> throwMine $ printf ("No right x found at warp cmd " ++
                        " at %s") (simpleShowLoc loc)


xSeparationWarning :: Loc -> Loc -> Bool
xSeparationWarning (Loc msr1 _) (Loc msr2 _) = msr1 <= msr2 - 2


hasX :: [Text] -> Bool
hasX ts = any (== "x") [s | TechniqueText s <- ts]


parseCmd :: Loc -> Parser ParsedCmd
parseCmd loc = 
  try (parsePause loc) 
  <|> 
  try (parseWarp loc)
  <|>
  try (parseArp loc)
  <|>
  try (parseStacc loc)


parseInt :: Loc -> Parser Int
parseInt loc = do
  s <- many1 (oneOf "-0123456789")
  let i = case reads s of
        [] -> throwMine $ printf ("in command technique text at %s, "
              "malformed integer '%s'") (simpleShowLoc loc) s
        (x,_):_ -> x
  return i


parseArp loc = do
  string "arp"
  Arp <$> parseInt loc


parseStac loc = do
  string "stac"
  Stac <$> parseInt loc


parsePause loc = do
  string "ps"
  Pause <$> parseInt loc


parseWarp loc = do
  left <- optionMaybe $ char '/'
  i <- parseInt loc
  right <- optionMaybe $ char '/'
  eof
  return $ Warp (isJust left) (isJust right) i
-}
