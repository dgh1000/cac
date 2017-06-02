module Score.Text where

   ... this module not being used now ...


import qualified Data.Map as M
import qualified Data.List as L
import Control.Arrow
import Control.Monad
import Debug.Trace
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Data.Map(Map)
import Data.Maybe
import Common.CommonData
import Common.CommonUtil
import Score.ScoreData
import Util.Map(listToLMap,lMapToList)
import Util.Exception


data ParsedCmd = Pause Int
               | Warp Bool Bool Bool Int  -- <staff flag> <is left warp>
                                          -- <is right warp> <amt>
               | Stac Int
               | Arp Int



computeTextCmds :: Map Loc [Text] -> Map Loc [TextCmd]
computeTextCmds text = listToLMap . mapMaybe (maybeMakeCmd text) $
  [(loc,s) | (loc,TechniqueText s) <- lMapToList text]
  

maybeMakeCmd :: Map Loc [Text] -> (Loc,String) -> Maybe (Loc,TextCmd)
maybeMakeCmd text (loc,s) = case parse (parseCmd loc) "" s of
  Left _ -> Nothing 
  Right (Pause i)    -> Just (loc, PauseCmd $ fromIntegral i / 100)
  Right (Warp s l r i) -> Just (loc, computeWarp text loc s l r i)
  Right (Stac i)     -> Just (loc, StacCmd $ fromIntegral i / 100)
  Right (Arp i)      -> Just (loc, ArpCmd $ fromIntegral i / 100)


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


parseInt :: Loc -> Parser Int
parseInt loc = do
  s <- many1 (oneOf "-0123456789")
  let i = case reads s of
        [] -> throwMine $ printf ("in command technique text at %s, " ++
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
  left    <- isJust `liftM` optionMaybe (char '/')
  isStaff <- isJust `liftM` optionMaybe (char 's')
  i       <- parseInt loc
  right   <- isJust `liftM` optionMaybe (char '/')
  eof
  when (not left && not right) (throwMine $ printf ("warp at %s must have" ++
       " at least one side") (simpleShowLoc loc))
  when (isStaff && not (left && right)) (throwMine $ printf ("staff warp " ++
       "at %s must be double-sided") (simpleShowLoc loc))
  return $ Warp isStaff left right i


