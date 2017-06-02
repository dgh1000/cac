{-

to do:

  remove redundant algo

    got it

  check for, among dests

    bk-man-opt/PURCHASED-SAMPLES/Garritan/Garritan_Personal_Orchestra_4/
      SamplesData/ ... doesn't match anything in dests. Not sure if the source
      files referred to here are in a zip folder, or "in the open"

      yeah in destimation they are in zip folder only

    I'm not using software to check for all source p occurring in dests. might
    want to do manual look at /users/mike/p

      okay I've done enough to be satisfied

    make sure I have Earmaster in archive/software

      actually, have it in archive/Downloads-to-save
      

    I notice that "MP3 Open Focus.zip" in sources does not match
    anything. probably this is because I'm archiving only the unzipped files,
    but check anyway that all Open Focus files are archived

      it's in core/buddhism-meditation -- but I just moved to archive

    make sure we've archived Soliloquy Viola .. right now the sources in
    bk-man\concert-video are not showing in the dests

      okay I've got an audio recording in core/music/compositions

  look for duplicates in dests

    some duplicate or unnecessary audio recording data in "In the Moment" (in
    compositions, recordings-of-my-compositions)

      I don't understand what all these files are doing there, so I will just
      leave it that way

    Fonts_from_HP

      don't worry about it, these don't take up much space, and will never be
      changed

    America's Family Pet Expo

       don't worry about it--they won't be changed

    looks like certain music ended up in both archive/music/sound-files and
    archive/audio-recordings/

       okay, can't find the duplicates, but I reorganized a bit by moving
       archive/music/sound-files to archive-music-recordings

    looking at root of tree bk-man/Thunderbird-Profiles-local-mail/ we seem to
    find many of the files in that tree at both
    archive/email/THUNDERBIRD-LOCAL-FOLDERS and
    archive/email/Thunderbirid-Profiles-liocal-mail

       okay leave it alone

sources done

  crit

    double-checked    

  bk-auto-cloud

    double-checked

  bk-auto

    made first pass, created exceptions that got problem files down to 0

    some exceptions were maybe granting exceptions to actual problems

      in particular, some files in /bk-auto/misc-music may have been missing
      from ~/archive/

    moved some music from /bk-auto/misc-music to archive

    deleted some of the exceptions

    need to re-run comparison

    okay, re-ran comparison and got problems down to zero

  bk-man-opt

    first pass: added exceptions, got reports down to zero

    gave second look to exceptions: don't see any problems. 

    done.

  bk-man

    first pass, got reports down to zero

    gave second look to exceptions: don't see any problems

    reran because of other changes to dests since first pass: had to add one
    more exception, then got reports down to zero

    done

-}


import Data.Text.Lazy as TL
import Util.DataReorg.CmpSpecs


dirContainsList s ds = [ExcDirContains (s++x) | x <- ds]

pre_bk_man x = "sourceSpecs\\bk-man\\" ++ x

bk_man = Config
 { cSources = [ pre_bk_man "bittorrent-downloads.txt"
              , pre_bk_man "concert-video.txt"
              , pre_bk_man "feldenkrais-atm-from-lee.txt"
              , pre_bk_man "Ipod_backup3.txt"
              , pre_bk_man "mark-reese-atm.txt"
              , pre_bk_man "Mathematics.txt"
              , pre_bk_man "MP3 Open Focus.txt"
              , pre_bk_man "original-XCE-instruments.txt"
              , pre_bk_man "other-atm.txt"
              , pre_bk_man "other-atm-2.txt"
              , pre_bk_man "paris-arts-atm.txt"
              , pre_bk_man "ralph-atm.txt"
              , pre_bk_man "Thunderbird-Profiles-local-mail.txt"
              ]
 , cMatch0    = [ ExcDirContains "gold-backup\\decided"
                -- we're not saving this as we have no intent on putting it
                -- back on iPod, don't have anohter use for all these files
                -- because they are mp3 or apple formats
                , ExcDirContains "Ipod_backup3"
                -- this is moved to Passport music
                , ExcDirContains "The Well-Tempered Synthesizer" 
                -- no problem with this exception: all Wendy Carlos is moved
                -- to Passport music (beauty in beast, two Bach albums and
                -- 'well-tempered synth'
                , ExcDirContains "Wendy_CARLOS_"
                -- P is already verified
                , ExcDirContains "P\\no-optical-backup"
                , ExcDirContains "P\\gold-backup"
                -- already on Passport
                , ExcDirContains "Beauty In The Beast"
                -- Earmaster Pro is already in archive/downloads-to-save
                , ExcDirContains "Earmaster School"
                -- not saving following seven: don't want to use pirtaed
                -- software
                , ExcDirContains "Arturia Moog Modular"
                , ExcDirContains "Miroslav Philarmonik"
                , ExcDirContains "DRPATJE-Gigastudio3"
                , ExcDirContains "Reaper 3.14.1"
                , ExcDirContains "IK.Multimedia.Miroslav.Philharmonik"
                , ExcDirContains "Cycling.74.MAXMSP"
                , ExcFilename "Earmaster.School.5.0.615S-TE.rar"
                -- already verified Open Focus is in archive
                , ExcFilename "MP3 Open Focus.zip"
                -- not copying, anyway P is already organized
                , ExcFilename "not-staging-Klixen a-hj116 A Moist Visit-not-staging.wmv"
                , ExcFilename "gold-backup-notes.txt"
                -- already verified recording is in
                -- core/music/compositions.. recordings
                , ExcDirContains "Soliloquy-Viola"
                , ExcDirContains "Earmaster.School"
                ]
 , cExactGT1  = [ ExcDirContains "Thunderbird-Profiles-local-mail" ]
 , cFnameOnly = [ ExcDirContains "Ipod_backup3"
                , ExcDirContains "Earmaster.School"
                , ExcDirContains "Arturia Moog Modular"
                , ExcFilename "Thumbs.db"
                , ExcDirContains "DRPATJE-Gigastudio3"
                ]
 }




preCrit x = "sourceSpecs\\crit-done-moving\\" ++ x


crit = Config
  { cSources = [ preCrit "crit-ahk.txt", preCrit "crit-code.txt"
               , preCrit "crit-Lisp.txt", preCrit "crit-Music.txt"
               , preCrit "crit-python-tutoring.txt"
               , preCrit "crit-websites.txt"
               , preCrit "crit-docs.txt" ]
  , cMatch0    = [ ExcDirContains  "\\code\\old-haskell"
                 , ExcDirContains  "\\code\\old-py"
                 , ExcDirContains  "\\code\\old-teach"
                 , ExcDirContains  "\\Music\\lilypond"
                 , ExcDirContains  "\\Music\\csound"
                 , ExcFilename  "artsurveillance.zip"
                 ]
  , cExactGT1  = [ ExcDirContains  "\\Music\\algo"
                 , ExcDirContains  "\\not_gigasampler"
                 , ExcDirContains  "\\Music\\early_csound_algo"
                 , ExcDirContains  "\\2016\\faster"
                 , ExcDirContains  "\\compositions\\2015"
                 , ExcDirContains  "\\compositions\\2016"
                 , ExcDirContains  "\\compositions\\Algo"
                 , ExcDirContains  "\\ahk\\autotype"
                 , ExcFilename  "p.hs"
                 , ExcFilename  "r.hs"
                 , ExcFilename  "csd.csd"
                 , ExcFilename  "debug.txt"
                 , ExcFilename  "bza_final.ai"
                 , ExcFilename  "complis.zip"
                 , ExcDirContains  "Disabilty-2010"
                 , ExcDirContains  "Tex-experiment"
                 , ExcDirContains  "\\Health\\ebooks"
                 , ExcDirContains  "\\docs\\ebooks"
                 ]
  , cFnameOnly = [ ExcFilename  "debug.txt"
                 -- all of these music files, makes sense that they would
                 -- have changed modification time. But is there a source
                 -- file X that doesn't exist in the dest, because the
                 -- mathcing filename Y is the dest is not the same file?
                 -- impossible with musical scores.
                 , ExcFilename  "fast-piano-02.sib"
                 , ExcFilename  "faster-02-B.sib"
                 , ExcFilename  "faster-01-A.sib"
                 , ExcFilename  "piano.RPP"
                 , ExcFilename  "piano.RPP-bak"
                 , ExcFilename  "startup.ahk"
                 , ExcDirContains  "\\2016\\faster"
                 , ExcDirContains  "\\2015\\fast"
                 , ExcDirContains  "\\2015\\slane"
                 , ExcDirContains  "\\Music\\lilypond"
                 , ExcDirContains  "\\2016\\waifie-life"
                 ]
  }


bk_auto_cloud = Config
  { cSources = ["sourceSpecs\\bk-auto-cloud\\images.txt"
               , "sourceSpecs\\bk-auto-cloud\\Mike-less-crit.txt"
               ]
                 -- this is not important
  , cMatch0    = [ ExcDirContains  "website-as-of"
                 -- okay this was moved to p
                 , ExcDirContains  "images\\moved-p"
                 , ExcFilename  "exercise.ai"
                 , ExcDirContains  "\\student-id"
                 , ExcDirContains "\\Proj\\haskell"
                 , ExcFilename "20070917-Jack_Kornfield-SR-the_feminine_in_buddhism.mp3"
                 , ExcFilename "20090629-Jack_Kornfield-SR-wise_thought_freedom_with_bhante_buddharakkhita.mp3"
                 , ExcFilename "20091109-Jack_Kornfield-SR-the_perfection_of_truthfulness.mp3"
                 , ExcDirContains "beautiful_agony"
                 , ExcFilenameContains "beautiful agony"
                 , ExcDirContains "Piano-audition"
                 , ExcDirContains "metta-music"
                 , ExcDirContains "code-pre-transfers"
                 , ExcFilenameContains "Love and Wisdom.zip"
                 , ExcFilenameContains "-Meditation.zip"
                 , ExcDirContains "\\Proj\\Perl"
                 , ExcDirContains "JPL"
                 , ExcDirContains "\\Proj\\qt_exper"
                 , ExcDirContains "\\Proj\\old_ahk"
                 ]
                 ++
                 dirContainsList "Buddhsim\\" [ "Cohn"
                                              , "Goldstein"
                                              , "Lila-Kate-Wheeler"
                                              , "Noah Levine"
                                              , "Sally Clough-Armstrong"
                                              , "Salzberg\\dharma seed"
                                              ]
  , cExactGT1  = [ ExcDirContains  "Fonts_from_HP"
                 , ExcFilename  ".BridgeSort"
                 , ExcDirContains  "Americas-Family"
                 , ExcDirContains  "THUNDERBIRD-LOCAL-FOLDERS"
                 , ExcDestDirContains  "jpegs-for-Heather"
                 , ExcDirContains  "\\photos\\Family"
                 , ExcDirContains  "\\Mike-less-crit\\Artistic"
                 , ExcDirContains  "\\scrapbooking"
                 , ExcDirContains  "\\equipment_photos"
                 , ExcDirContains  "\\pets\\rats"
                 , ExcDirContains  "\\pets\\bunnies"
                 , ExcDirContains  "\\scanned_family"
                 , ExcDirContains  "\\full-session-in-the"
                 , ExcDirContains  "\\csound_lime"
                 , ExcDirContains  "\\musical-playback-synth"
                 , ExcDestDirContains  "files-from-alumnus"
                 , ExcDirContains "mikesweb"
                 , ExcDirContains "Web design"
                 , ExcDestDirContains "Web design"
                 , ExcDirContains "first_io"
                 , ExcDirContains "\\WORK"
                 ]

  , cFnameOnly = [ ExcDirContains  "website-as-of-2012-06"
                 , ExcDirContains  "mikesweb2"
                 , ExcDirContains  "code-pre-transfers"
                 , ExcDirContains  "old_ahk"
                 , ExcDirContains  "early_csound_algo"
                 , ExcDirContains  "\\WORK"
                 , ExcDirContains  "\\first_io"
                 , ExcDirContains  "qt_exper"
                 , ExcDirContains  "web_dec_2003"
                 , ExcDirContains  "emacs_time_saving"
                 , ExcDirContains  "metta-music"
                 , ExcDirContains  "Web design"
                 , ExcDirContains  "\\Proj\\potm"
                 , ExcDirContains  "\\Proj\\photo_tools"
                 , ExcDirContains  "\\test_signal"
                 , ExcDirContains  "\\Tax\\2008"
                 , ExcDirContains  "\\Tax\\2007"
                 , ExcDirContains  "moved-p"
                 , ExcDirContains  "\\Proj\\Perl"
                   -- if a filename is commonly used for temp or generic
                   -- files, then it might occur with different time tags,
                   -- meaning it really is completely different data. we need
                   -- to evaluate for each of the following file names: could
                   -- anything important really be named this?
                 , ExcFilename  "Thumbs.db"
                 , ExcFilename  "NOTES.TXT"
                 , ExcFilename  "NOTES.TXT~"
                 , ExcFilename  "README.TXT"
                 , ExcFilename  "README.txt"
                 , ExcFilename  "README.TXT~"
                 , ExcFilename  "notes.doc"
                 , ExcFilename  "notes.txt"
                 , ExcFilename  "Makefile"
                 , ExcFilename  "notes.txt~"
                 , ExcFilename  "__init__.py"
                 , ExcFilename  "out.txt"
                 , ExcFilename  "quiz.txt"
                 , ExcFilename  "quiz.txt~"
                 , ExcFilename  "script.hs"
                 , ExcFilename  "script.hs~"
                 , ExcFilename  "script.py"
                 , ExcFilename  "script.py~"
                 , ExcFilename  "test.py"
                 , ExcFilename  "test.py~"
                 , ExcFilename  "test.ahk"
                 , ExcFilename  "test.ahk~"
                 , ExcFilename  "test.txt"
                 , ExcFilename  "test.txt~"
                 , ExcFilename  "test2.html"
                 , ExcFilename  "1.jpg"
                 , ExcFilename  "2.jpg"
                 , ExcFilename  "3.jpg"
                 , ExcFilename  "4.jpg"
                 , ExcFilename  "5.jpg"
                 , ExcFilename  "6.jpg"
                 , ExcFilename  "7.jpg"
                 , ExcFilename  "8.jpg"
                 , ExcFilename  "9.jpg"
                 , ExcFilename  "01.jpg"
                 , ExcFilename  "02.jpg"
                 , ExcFilename  "03.jpg"
                 , ExcFilename  "04.jpg"
                 , ExcFilename  "05.jpg"
                 , ExcFilename  "06.jpg"
                 , ExcFilename  "07.jpg"
                 , ExcFilename  "08.jpg"
                 , ExcFilename  "09.jpg"
                 , ExcFilename  "10.jpg"
                 , ExcFilename  "11.jpg"
                 , ExcFilename  "12.jpg"
                 , ExcFilename  "13.jpg"
                 , ExcFilename  "14.jpg"
                 , ExcFilename  "15.jpg"
                 , ExcFilename  "16.jpg"
                 , ExcFilename  "17.jpg"
                 , ExcFilename  "18.jpg"
                 , ExcFilename  "20.jpg"
                 , ExcFilename  "brain.zip"
                 ]

  }


bk_auto = Config
 { cSources   = ["sourceSpecs\\bk-auto.txt"]
                -- okay definitely don't need to save old-algo-ahk
 , cMatch0    = [ ExcDirContains  "old-algo-ahk" 
                -- burnt by bunny is moved to my passport music backup
                , ExcDirContains  "burnt by bunny"
                -- archive/math contains plenty of textbooks
                , ExcDirContains  "math-textbooks"
                -- this isn't important anyway
                , ExcDirContains  "pod_rescue_permanent"
                , ExcFilename  "gold-backup-notes.txt"
                , ExcFilename  "SyncToy_6b91b3cb-56da-4769-83c1-9624716f1cde.dat"
                , ExcDirContains "misc-iTunes\\AIR"
                , ExcDirContains "Leonard Slatkin_"
                -- moved this to Passport folder music
                , ExcDirContains "Jay-Greenberg"
                ]
 , cExactGT1  = [ ExcDirContains  "old-algo-ahk"
                , ExcDirContains  "Amazon MP3"
                , ExcDirContains  "The Swingin"
                , ExcDirContains  "Made In Italy"
                , ExcDirContains  "\\Downloads-to-save\\fonts"
                , ExcDirContains  "FlexGrocer"
                , ExcDirContains  "new-rev-slider-proj"
                , ExcDirContains  "\\csun"
                , ExcFilename  "Goudy Old Style Bold BT.ttf" 
                , ExcFilename  "RUTAN___.TTF"
                , ExcDirContains "Semblance"
                , ExcFilenameContains "Calabria"
                , ExcDirContains "\\AIR"
                , ExcDirContains "Vietas family"
                ]
 , cFnameOnly = [ ExcDirContains  "old-algo-ahk" 
                , ExcDirContains  "\\math-texts"
                , ExcFilename  "Fast_Track_Pro_Installer_6_0_7_77137.zip"
                ]
 }


bk_man_opt = Config
  { cSources    = [ "sourceSpecs\\bk-man-opt.txt" ]
  , cMatch0     = [ ExcDirContains  "Garritan_Personal_Orchestra_4" 
                  , ExcDirContains  "Sibelius Sounds 7"
                  , ExcDirContains  "\\sibelius\\sibelius7"
                  , ExcFilename  "Sibelius62Full_66417.exe"
                  ]
  , cExactGT1   = []
  -- so we have a source file S that matches several filenames in the dest Ds
  -- but not any of their dates.
  --
  -- scenarios where that is okay
  --
  --   - among Ds is a later version of S, and we only need the later version
  --
  --   - S just doesn't matter anyway, not important
  --
  --     - something that isn't actually my data, like __init__.py, Thumbs.db
  --
  --     - something I don't care to save, like old scrapbooking course data,
  --       or a music download that I don't need to save because I have it
  --       somewhere else
  --
  --   - 
  , cFnameOnly  = [ ExcFilename ".DS_Store"
                  ]
 }

dests   = [ "destSpecs\\archive.txt"
          , "destSpecs\\core.txt"
          , "destSpecs\\OneDrive.txt"]

main = cmpSpecs bk_man dests
