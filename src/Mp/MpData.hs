
module Mp.MpData where

import Translation.TranslationData

-- type MsrRange = (Int, Maybe Int)

{-
data PlayArg = PlayArgMsrRange (Int,Maybe Int) (Maybe Char)
             -- ^ Maybe Char is splice point
             | PlayArgQuit
             | PlayArgTermProc
             | PlayArgCwd         Int
             | PlayArgShowDirs 
             | PlayArgChangeTempo Double
             | PlayArgTranspose   Int
               deriving(Eq,Show)
-}

-- Expressing result of parsing the input line
data InpCmd  = InpQuit
             | InpPlay (Int,Maybe Int) (Maybe Char) 
             -- ^ Maybe Char is splice point
             | InpChangeTempo Double
             | InpParseError String
             | InpTerminateProcess
             | InpShowDirs
             | InpCwd Int          -- change working directory to
                                   -- one of hard-coded directories
             | InpTranspose Int
             deriving(Show)

{-
data StaffOption = MuteOption MuteState
                 | SusPedOption SusPedUse
-}
