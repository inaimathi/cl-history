module FactBase where

import History
import Data.Set (Set, empty)
import qualified Data.Set as Set

data FactVal = Str String
             | Num Integer
             | Lst [FactVal] deriving (Eq, Ord, Show, Read)
data Fact = Fact { fid :: Integer, key :: FactVal, val :: FactVal } deriving (Eq, Ord, Show, Read)

data Base = Base { nextId :: Integer, facts :: Set Fact } deriving (Eq, Ord, Show)

makeBase :: Archive Base Fact
makeBase = Archive { into = (\base f -> base { facts = Set.insert f $ facts base
                                             , nextId = succ $ max (fid f) (nextId base)})
                   , outof = (\base f -> base { facts = Set.delete f $ facts base })
                   , state = Base 0 empty }

newEv :: Archive Base Fact -> Event Fact -> IO (Archive Base Fact)
newEv base ev = newEventToFile base "fact-base.arc" ev

insert :: Archive Base Fact -> Fact -> IO (Archive Base Fact)
insert base = newEv base . Into

delete :: Archive Base Fact -> Fact -> IO (Archive Base Fact)
delete base = newEv base . Outof

insertNew :: Archive Base Fact -> FactVal -> FactVal -> IO (Archive Base Fact)
insertNew base b c = newEv base . Into $ Fact (nextId $ state base) b c

insertMulti :: Archive Base Fact -> [(FactVal, FactVal)] -> IO (Archive Base Fact)
insertMulti base fs = recur fs base
    where a = (nextId $ state base)
          recur [] acc = return $ acc
          recur ((b, c):rest) acc = do res <- insert acc $ Fact a b c
                                       recur rest res

main :: IO (Archive Base Fact)
main = loadFromFile makeBase "fact-base.arc"
