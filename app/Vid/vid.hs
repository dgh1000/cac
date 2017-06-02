
import System.Cmd
import Text.Printf
import Text.Parsec.ByteString
import Text.Parsec
import Util.Exception
import qualified Vid.VidData as VD
import Vid.VidData( RawSceneEntry(..)
                  , SceneEntry(..)
                  , Elem
                  , Font(..) )
import qualified Data.ByteString as B
import Vid.ParseScene
import Vid.Elem



main = do
  -- configuration
  let stringsFileName = "c:/haskell/App/Pyvid/strings.txt"
      metricsFileName = "c:/haskell/App/Pyvid/metrics.txt"
      sceneFileName = "c:/haskell/App/Vid/sceneFile.txt"

  -- parse scene file
  sceneFile <- readSceneFile sceneFileName
  -- write strings file
  writeStringsFile stringsFileName sceneFile
  -- run python program to determine string metrics
  system "python.exe c:/App/Pyvid/findTextMetrics.py"
  {-
  -- read metrics file
  stringMetrics <- readMetricsFile metricsFileName
  -- write showable form of metrics file
  putFile "out.xt" (showi . show $ stringMetrics)
  -}

readSceneFile :: String -> IO [SceneEntry]
readSceneFile sceneFileName = do
  buf <- B.readFile sceneFileName 
  case parse parseScene "" buf  of
    Left err -> do
       putStrLn $ show err
       throwMine "lksjdf"
    Right x -> return $ convertSceneEntries x

convertSceneEntries :: [RawSceneEntry] -> [SceneEntry]
convertSceneEntries = map g
  where
    g (RSEInstr i) = SEInstr i
    g (RSEDataElem e) = SEElem $ toElemFromList e


-- writeStringsFile
--  Write the file containing a list of entries, each one containing
--  text and font names/sizes. 
-- Example entry in strings file:
--  Times New Roman,16,false,false,Hello
-- That is,
--  <font name>,<font size>,<bold>,<italic>,<text>
-- Text is terminated by an end of line, so it cannot contain line endings
-- within it.
writeStringsFile :: String -> [SceneEntry] -> IO ()
writeStringsFile sFileName sceneFile = do
  let ss = extractStringsFromEntries sceneFile
  writeFile sFileName (computeStringsFile ss)

{- 
extractStrings :: [RawSceneEntry] -> [(String,Font)]
extractStrings vidFile = catMaybes . map f . vfEntries $ vidFile
  where
    f :: RawSceneEntry -> Maybe (String,Font)
    f (RSEInstr _) = Nothing
    f (RSEDataElem (DEText s _)) = 
        Just (s, Font "Times New Roman" 16 False False)
-}

computeStringsFile :: [(String,Font)] -> String
computeStringsFile = concatMap g
  where
    g :: (String,Font) -> String
    g (s, Font name size bold italic) =
      printf "%s,%s,%s,%s\n" name (show size) (show bold) (show italic)



{-
-- parseTokenText
--   Parse legal token text, which is any character except '[', ']', and
--   whitespace.
parseTokenText = do
  manyTill anyChar (char '[' <|> char ']' <|> space)

-- parseVidToken
--   Parse a token, which can be one of three types:
--     Tag: starts with a $. Remaining characters can be anything except
--           '[' or ']' or whitespace
--     Label: starts with a / or //. Remaining characters can be anything
--            except ']' or '[' or whitespace
--     Text: anything that's not a tag or label. can contain any characters
--              except ']' or '[' or whitespace
--     
parseVidToken :: Parser VidToken
parseVidToken = do
  skipMany space
  l <- ((do try (string "//")
            s <- parseTokenText
            return (VidLabel s True))
        <|> 
        (do char '/'
            s <- parseTokenText
            return (VidLabel s False))
        <|> 
        (do char '$'
            VidTag <$> parseTokenText)
        VidText <$> parseTokenText)
  skipMany space
  return l


-- parseVidTokenAlphaNum
--   Parse a token of "alphanum" type, which means a token that has only
--   alphanumeric characters. 
parseVidTokenAlphaNum :: Parse String
parseVidTokenAlphaNum = do
  skipMany space
  s <- many1 alphaNum
  skipMany space
  return s

parseEntry :: Parse VidEntry
parseEntry = do
  char '['
  skipMany space
  out <- ((do try (string "set")
              skipMany space
              s <- parseVidTokenString
              ts <- many1 parseVidToken
              return $ VEInstr (VISet s ts))
          <|>
          VidElem <$> parseVidTokenString)
  skipMany space
  char ']'
  skipMany space
  return out
   

parseVidFile :: Parse [VidEntry]
parseVidFile = many1 parseEntry

data Font = Font
  { fontName :: String
  , fontSize :: Int
  , fontBold :: Bool
  , fontItalic :: Bool
  }

data VidFile = VidFile {
  vfEntries :: [VidEntry]
  }
  
data VidEntry = VEInstr VidInstr
              | VEElem VidElem

data VidToken = VidTag String
              | VidLabel String Bool -- Bool is true for a double-slash label
                                     -- also known as narrow label
              | VidText String

data VidInstr = VISet String [VidToken]

data VidElem = VidElem String
-}
