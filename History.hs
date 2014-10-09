module History ( Event(..), Archive(..)
               , applyEvent, applyMany
               , readEvents, loadFrom, loadPartialFrom, loadFromFile
               , newEvent
               , fileWrite, streamsWrite ) where

import System.IO
import System.Time

data Event a = Ev CalendarTime a deriving (Eq, Read, Show)

data Archive a b = Archive { into :: (a -> b -> a)
                           , write :: (Event b -> IO ())
                           , state :: a }
instance Show a => Show (Archive a b) where
    show arc = concat ["Archive { ", show (state arc), " }"]

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

newEvent :: Show b => Archive a b -> b -> IO (Archive a b)
newEvent arc b = do t <- getClockTime
                    let ev = Ev (toUTCTime t) b
                    do _ <- (write arc) ev
                       return $ applyEvent arc ev

streamsWrite :: Show b => [Handle] -> Event b -> IO ()
streamsWrite handles event = mapM_ (\h -> hPutStrLn h ev) handles
    where ev = show event

fileWrite :: Show b => FilePath -> Event b -> IO ()
fileWrite fname ev = withFile fname AppendMode newEv
    where newEv handle = streamsWrite [handle] ev
