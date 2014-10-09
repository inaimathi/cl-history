module ToyAccount where

import History

data Account = Acc Int deriving (Eq, Ord, Show)

makeAccount :: Archive Account Int
makeAccount = Archive { into = (\(Acc balance) d -> Acc $ balance + d)
                      , write = fileWrite "account.arc" 
                      , state = Acc 0 }

main :: IO (Archive Account Int)
main = loadFromFile makeAccount "account.arc"
