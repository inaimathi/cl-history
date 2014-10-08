module History ( Event(..), Archive(..)
               , applyEvent, applyMany
               , readEvents, loadFrom, loadPartialFrom, loadFromFile
               , newEvent, newEventToStreams, newEventToFile ) where

import System.IO
import System.Time

data Event a = Ev CalendarTime a deriving (Eq, Read, Show)

data Archive a b = Archive { into :: (a -> b -> a), state :: a }
instance Show a => Show (Archive a b) where
    show arc = "Archive { " ++ show (state arc) ++ " }"

applyEvent :: Archive a b -> Event b -> Archive a b
applyEvent arc (Ev _ b) = arc { state = (into arc) (state arc) b }

applyMany :: Archive a b -> [Event b] -> Archive a b
applyMany arc = foldl applyEvent arc

readEvents :: Read b => Archive a b -> Handle -> IO [Event b]
readEvents _ = fmap (map read . lines) . hGetContents

loadPartialFrom :: Read b => Archive a b -> Handle -> Int -> IO (Archive a b)
loadPartialFrom arc handle ct = fmap (applyMany arc . take ct) $ readEvents arc handle

loadFrom :: Read b => Archive a b -> Handle -> IO (Archive a b)
loadFrom arc handle = fmap (applyMany arc) $ readEvents arc handle

loadFromFile :: Read b => Archive a b -> FilePath -> IO (Archive a b)
loadFromFile arc fname = fmap (applyMany arc . map read . lines) $ readFile fname

newEvent :: Show b => Archive a b -> (Event b -> IO ()) -> b -> IO (Archive a b)
newEvent arc fn b = do t <- getClockTime
                       let ev = Ev (toUTCTime t) b
                       do _ <- fn ev
                          return $ applyEvent arc ev

newEventToStreams :: Show b => Archive a b -> [Handle] -> b -> IO (Archive a b)
newEventToStreams arc handles = newEvent arc write
    where write ev = mapM_ (\h -> hPutStrLn h $ show ev) handles

newEventToFile :: Show b => Archive a b -> FilePath -> b -> IO (Archive a b)
newEventToFile arc fname ev = withFile fname AppendMode newEv
    where newEv handle = newEventToStreams arc [handle] ev
