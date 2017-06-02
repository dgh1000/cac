
import Control.Monad.ST
import Control.Monad
import Data.STRef

{-

do extension/truncate in MidiEvent rather than when working with NoteKeys

  advantages

    will work with notes of any source: regular, trill/trem, arpeggio

    will simplify what instruments do in time generation, which is good
    because there are many instruments

  algorithm capabilities

    extend notes for legato reasons or to make trills sound better

    truncate notes when repeated

      prevent midi off from one note affecting subsequent note

      provide separation between repetitions for non-percussive instruments

  strategy

    MidiEvent are input as Map Integer [MidiEvent]

    is there any drawback to changing midi times to Integer? they would still
    be computed as double in instruments and in Translation.hs, being
    converted to Integer only when creating the event.

    should I store both NoteEvent and SingleEvent?

      with tOff-modify we can just ignore SingleEvent. but useful to have it
      there for cut

    how should this interact with splicing?

      is there an advantage to using Map form with splicing code? faster
      splitting into events at cut boundaries for one thing. I think not much
      else advantage

      which should be done first, splicing or tOff-modify? I think doing
      splicing first means notes with end times exactly at boundaries will be
      recognizable as such. wouldn't want to do an extension from inside the
      cut to outside it

    do I handle trill/trem differently than regular notes? should I mark the
    midi events with data to distinguish? no need immediately to handle
    trilltrem differently, so I can skip it for now 

    what configuration data is needed for this algorithm?

      all instruments will use a 0.002 truncate for following repeated notes,
      so that doesn't need to be configured

      require truncation? indicate amount, and min dur after truncation

      require extension? indicate max amount, and min separation from any
      following repeated notes

      do midi events need to be tagged with any new information? probably
      not. algorithm will apply uniformly to all notes within a given staff

 -}


{-

storing mod events separately from note-produced and pedal events

  goal: eliminate need to sort them before playback

  could store as array, but main issue is to have them in order

-}


{-

algorithm for streaming midi

  input format: 

    several lists of mod events: for each instrument that uses mod

    list of note-related and pedal events for each instrument

    need an algorithm that merges several lists by selecting from among them

-}


{-
adjustEnds :: Map Double [MidiEvent]
adjustEnds = 
  where
    desc = 
-}

sumST :: Num a => [a] -> a
sumST xs = runST $ do
  n <- newSTRef 0
  forM_ xs $ \x -> do
    modifySTRef n (+x)
  readSTRef n

x1 = sumST [1,2,3,4] :: Int



-- we need to decide which list has the earliest event, and remove from that
-- list and pass in a loop. in two passes? check which is earliest, then remove
-- from each list all those equal to it?

merge :: Ord k => [[(k,a)]] -> [(k,a)]
merge ls
  | null fs = []
  | otherwise = 
      let m = minimum . map fst $ fs
          nexts = concatMap (takeWhile ((==m) . fst)) ls
          remains = map (dropWhile ((==m) . fst)) ls
      in nexts ++ merge remains
  where
    fs = concatMap (take 1) $ ls
    

xs1 = [ [(3,'d')]
      , [(1,'a'),(1,'b'),(5,'c')]
      , []
      ]

