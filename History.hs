module History where

data Event a = In a
             | Out a deriving (Eq, Read, Show)

data Archive a b = Archive { tick :: (a -> b -> a)
                           , wind :: (a -> b -> a)
                           , empty :: a }

applyEvent :: Archive a b -> a -> Event b -> a
applyEvent arc a (In b) = (tick arc) a b
applyEvent arc a (Out b) = (wind arc) a b

loadFrom :: Read b => Archive a b -> FilePath -> IO a
loadFrom arc fname = do evs <- fmap (map read) . fmap lines $ readFile fname
                        return $ foldl (applyEvent arc) (empty arc) evs

data Account = Account { balance :: Integer } deriving (Eq, Ord, Show)

accArc :: Archive Account Integer
accArc = Archive { tick = (\a t -> a { balance = balance a + t })
                 , wind = (\a t -> a { balance = balance a - t })
                 , empty = Account 0 }
