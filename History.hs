module History ( Event(..), Archive(..)
               , opposite, applyEvent, applyMany, rewindEvent, rewindMany
               , loadFrom, loadFromFile
               , newEvent, newEventToStreams, newEventToFile ) where

import System.IO

data Event a = Into a
             | Outof a deriving (Eq, Read, Show)

opposite :: Event a -> Event a
opposite (Into a) = Outof a
opposite (Outof a) = Into a

data Archive a b = Archive { into :: (a -> b -> a)
                           , outof :: (a -> b -> a)
                           , state :: a }
instance Show a => Show (Archive a b) where
    show arc = "Archive { " ++ show (state arc) ++ " }"

applyEvent :: Archive a b -> Event b -> Archive a b
applyEvent arc (Into b) = arc { state = (into arc) (state arc) b }
applyEvent arc (Outof b) = arc { state = (outof arc) (state arc) b }

applyMany :: Archive a b -> [Event b] -> Archive a b
applyMany arc = foldl applyEvent arc

rewindEvent :: Archive a b -> Event b -> Archive a b
rewindEvent arc = applyEvent arc . opposite

rewindMany :: Archive a b -> [Event b] -> Archive a b
rewindMany arc = foldl rewindEvent arc

loadFrom :: Read b => Archive a b -> Handle -> IO (Archive a b)
loadFrom arc handle = recur handle arc
    where recur h acc = do res <- hIsEOF h
                           if res
                           then return acc
                           else do ln <- hGetLine h
                                   recur h . applyEvent acc $ read ln

loadFromFile :: Read b => Archive a b -> FilePath -> IO (Archive a b)
loadFromFile arc fname = fmap (applyMany arc . map read . lines) $ readFile fname

newEvent :: Show b => Archive a b -> (Event b -> IO ()) -> Event b -> IO (Archive a b)
newEvent arc fn ev = do _ <- fn ev
                        return $ applyEvent arc ev

newEventToStreams :: Show b => Archive a b -> [Handle] -> Event b -> IO (Archive a b)
newEventToStreams arc handles = newEvent arc write
    where write ev = mapM_ (\h -> hPutStrLn h $ show ev) handles

newEventToFile :: Show b => Archive a b -> FilePath -> Event b -> IO (Archive a b)
newEventToFile arc fname ev = withFile fname AppendMode newEv
    where newEv handle = newEventToStreams arc [handle] ev
