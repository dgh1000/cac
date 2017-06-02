
import Util.DataReorg.GenSpecs




main = do
  let root = "\\Users\\Mike\\OneDrive"
      outputFile = "OneDrive.txt"
  genSpecs root outputFile

{-
main2 = do
  -- email, feldenkrais, files-from-alumnus, Fonts_from_HP, Haskell, health,
  -- Manuals, math, music, Personal_from_work, programming, software,
  -- surveillance
  let folders1 = [ "bittorrent-downloads"
                 , "concert-video"
                 --  , "c-root-stuff-before-win-7-and-new-mb"
                 , "feldenkrais-atm-from-lee"
                 , "Ipod_backup3"
                 , "mark-reese-atm"
                 , "Mathematics"
                 -- , "Mike-just-before-win7-and-new-mb"
                 , "MP3 Open Focus"
                 -- , "my-pictures-before-win7"
                 -- , "Open-Focus-Neuroprogrammer"
                 , "original-xce-instruments"
                 , "other-atm"
                 , "other-atm-2"
                 , "paris-arts-atm"
                 , "ralph-atm"
                 , "Thunderbird-Profiles-local-mail" ]
      g s = do
        let root = "d:\\bk-man\\" ++ s
            outputFile = s ++ ".txt"
        genSpecs root outputFile
  mapM_ g folders1
-}
