module ToyAccount where

import History

data Account = Acc Int deriving (Eq, Ord, Show)

makeAccount :: Archive Account Int
makeAccount = Archive { into = (\(Acc balance) d -> Acc $ balance + d)
                      , state = Acc 0 }

accountEvent :: Archive Account Int -> Int -> IO (Archive Account Int)
accountEvent arc = newEventToFile arc "account.arc"

main :: IO (Archive Account Int)
main = loadFromFile makeAccount "account.arc"
