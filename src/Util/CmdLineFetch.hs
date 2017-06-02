module Util.CmdLineFetch (cmdLineFetch) where

import System.IO
import System.Directory
import Control.Exception
import Text.Parsec
import Text.Parsec.String
import Util.Exception

-- cmdLineFetch
--   Read a "command line". Reprompt if
--       - the user enters nothing
--       - the parsec throws an exception or returns a parse error
--
-- Inputs
--   String: the prompt to use
--   Parser a : a Parsec parser function
--
cmdLineFetch :: String -> Parser a -> IO a
cmdLineFetch prompt p = do
  -- the following calls lineFetch2 in order to write the prompt, read and
  --   parse the response. 
  --     (1) exceptions may arise
  --        (a) type MyException
  --        (b) type IOException
  --     (2) parse errors may arise
  --     (3) a successful parse may happen and return valid data
  --  by wrapping this call to lineFetch2 in 'catches' we are able to 
  --  handle exceptions by printing their messages and then proceeding as
  --  a parse error of "" had been generated.
  d <- lineFetch2 prompt p `catches` allHandlersE
  case d of
    Left errMsg -> do
      putStrLn errMsg
      cmdLineFetch prompt p
    Right x -> return x


-- assists the 'cmdLineFetch' by writing the prompt, reading a line, and 
--  doing minimal interpretation (the only thing it does is re-call itself
--  upon finding an empty line). it doesn't try to catch errors.
lineFetch2 :: String -> Parser a -> IO (Either String a)
lineFetch2 prompt par = do
  d <- getCurrentDirectory
  putStrLn $"\n" ++ d ++ "\n"
  putStr prompt
  hFlush stdout
  li <- getLine
  if length li == 0
    then lineFetch2 prompt par
    else evaluate $ lineParse par li

-- call the parser on the line. Return "Left <errmsg>" if there is a parse
-- error, and "Right <value>" otherwise
lineParse ::  Parser a -> String -> Either String a
lineParse parseCmdLine s =
  case parse parseCmdLine "" s of 
    Left err -> Left (show err)
    Right cmd -> Right cmd

allHandlersE = [Handler myHandlerE, Handler ioHandlerE]

myHandlerE :: MyException -> IO (Either String a)
myHandlerE (MyException s) = do
  putStrLn $ "---\nERROR IN CMD LINE:\n---" ++ s
  return (Left "")

ioHandlerE :: IOException -> IO (Either String a)
ioHandlerE e = do
  putStrLn (show e)
  return (Left "")
