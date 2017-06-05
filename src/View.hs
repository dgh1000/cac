
module View where

import Text.XML.Light

toQ :: String -> QName
toQ s = QName s Nothing Nothing

e = Element (toQ "foo") [] [] Nothing
