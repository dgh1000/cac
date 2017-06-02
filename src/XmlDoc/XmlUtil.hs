module XmlDoc.XmlUtil where

import Debug.Trace
import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.List as L
import Text.Printf
import Text.XML.HaXml.Types
import Text.XML.HaXml.XmlContent
import Util.Exception
import Util.Showable
import XmlDoc.ShowHaXml

----------------------------------------------------------------------
----------------------------------------------------------------------
debugReport :: Element i -> String
debugReport elemIn = ("\n\n" ++ (showItem . showi $ elemIn)) `trace` ""

----------------------------------------------------------------------
----------------------------------------------------------------------
--              element util functions

-- looks at elements in the contents of the given input element.
-- enforces that there must be at least one.
processManyElems :: (Element i -> Maybe a) -> Element i -> [a]
processManyElems g elemIn@(Elem qname _ contents) = out
  {- if length out == 0
  then throwMine $ printf ("in processManyElems: should be at least " ++
           " one sub-element in '%s' %s") (qNameString qname)
           (debugReport elemIn)
  
  else out
  -}
  where
    out = catMaybes (map h contents)
    h (CElem elem _) = g elem
    h _ = Nothing

qNameString (N s) = s
qNameString (QN  _ s) = s

elemName (Elem name _ _) = qNameString name

elemContents (Elem _ _ contents) = contents

{-
-- Given a list of Content, look for all CElems with the given name
-- and return those elem's contents.
lookupElemContents :: String -> [Content i] -> [[Content i]]
lookupElemContents ename contents = out
  where
    out = catMaybes . map g $ contents
    g :: Content i -> Maybe [Content i]
    g (CElem elem _) = if elemName elem == ename
      then Just $ elemContents elem
      else Nothing
    g _ = Nothing
-}    

lookupElem :: String -> Element i -> Maybe (Element i)
lookupElem targetName elemIn = L.lookup targetName es
  where
    es = map (\e -> (elemName e,e)) . lookupElems targetName $ elemIn

-- look at the contents of an Element; compute a list of those contents
-- which are elements (i.e. CElem) and have the given name
lookupElems :: String -> Element i -> [Element i]
lookupElems targetName elemIn = catMaybes (map g (elemContents elemIn))
  where
    g (CElem elem _) = if elemName elem == targetName 
      then Just elem else Nothing
    g _ = Nothing


-- look at the contents of an Element; compute a list of those contents
-- which are elements (i.e. CElem) (no matter what name they have)
lookupElemsContent :: Element i -> [Element i]
lookupElemsContent elemIn = catMaybes . map g . elemContents $ elemIn
  where
    g (CElem elem _) = Just elem 
    g _ = Nothing


lookupEnforce1Elem :: String -> Element i -> Element i
lookupEnforce1Elem targetName elem = out
  where
    es = lookupElems targetName elem
    out = if length es /= 1
      then throwMine $ printf ("in lookupEnforce1Elem, didn't find " ++
           "just one elem with name '%s' as child of elem '%s'. %s")
           targetName (elemName elem) (debugReport elem)
      else head es

lookupEnforceZeroOrOneElem :: String -> Element i -> Maybe (Element i)
lookupEnforceZeroOrOneElem name elemIn =
  case lookupElems name elemIn of
    [] -> Nothing
    [e] -> Just e
    _   -> throwMine $ printf ("In lookupEnforceZeroOrOneElem, found more " ++
           "than one elem of name '%s' %s") name (debugReport elemIn)
    

enforce1CatMaybes err ms = 
  if length cms /= 1
  then throwMine err
  else head cms
  where
    cms = catMaybes ms

getEnforce1StringContent :: Element i -> String
getEnforce1StringContent elem 
  | length cs /= 1 = throwMine $ printf ("in getEnforce1StringContent, " ++
                     "not just one CString in elem '%s'") $ elemName elem
  | otherwise = head cs
  where
    cs = getStringContents elem

getEnforce1IntContent :: Element i -> Int
getEnforce1IntContent elemIn =
  case reads s of
    [(i,_)] -> i
    _ -> throwMine $ printf ("in getEnforce1IntContent, in elem named " ++
                             "'%s' there is not one integer as content " ++
                             "but instead there is '%s'") (elemName elemIn) s
  where 
    s = getEnforce1StringContent elemIn
             

getStringContents :: Element i -> [String]
getStringContents elem = catMaybes cs
  where
    cs = map g (elemContents elem)
    g (CString _ dat _) = Just dat
    g _ = Nothing

----------------------------------------------------------------------
----------------------------------------------------------------------
--                 attributes

lookupAttr :: String -> Element i -> Maybe String
lookupAttr attrName (Elem _ attrs _) =
  L.lookup attrName (map g attrs) 
  where
    g (aname,aval) = (qNameString aname,show aval)

lookupIntAttr :: String -> Element i -> Maybe Int
lookupIntAttr attrName elemIn = fmap g (lookupAttr attrName elemIn)
  where
    g s = case reads s of
      [(i,_)] -> i
      _ -> throwMine $ printf ("In lookupIntAttr, an attr '%s' was found but"++
                       " it's not an int (it's '%s')") attrName s
  

lookupEnforceAttr :: String -> Element i -> String
lookupEnforceAttr attrName elem =
  case lookupAttr attrName elem of
    Nothing -> throwMine $ printf ("didn't find attribute '%s' in element "++
               "'%s'") attrName (elemName elem)
    Just a -> a

getEnforceIntAttr :: String -> Element i -> Int
getEnforceIntAttr attrName elemIn = out
  where
    a = lookupEnforceAttr attrName elemIn
    out = case reads a of
      [(i,_)] -> i
      _ -> throwMine $ printf ("in getEnforceIntAttr, attribute '%s' in " ++
                       "element '%s' isn't an int") attrName (elemName elemIn)
