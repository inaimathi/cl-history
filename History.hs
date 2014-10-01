module History where

import System.IO

data Event a = In a
             | Out a deriving (Eq, Read, Show)

data Archive a b = Archive { tick :: (a -> b -> a)
                           , wind :: (a -> b -> a)
                           , empty :: a }

applyEvent :: Archive a b -> a -> Event b -> a
applyEvent arc a (In b) = (tick arc) a b
applyEvent arc a (Out b) = (wind arc) a b

loadFrom :: Read b => Archive a b -> Handle -> IO a
loadFrom arc handle = recur handle (empty arc)
    where recur h acc = do res <- hIsEOF h
                           if res
                           then return acc
                           else do ln <- hGetLine h
                                   recur h . applyEvent arc acc $ read ln

newEvent :: Show b => Archive a b -> a -> Event b -> [Handle] -> IO a
newEvent arc a ev handles = do _ <- mapM_ (\h -> hPutStrLn h $ show ev) handles
                               return $ applyEvent arc a ev
