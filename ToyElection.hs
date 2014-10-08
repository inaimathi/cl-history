module ToyElection where

import History

import Data.Map (Map, unionWith, fromList)
import qualified Data.Map as Map

data Election = Election { voteCount :: Int
                         , candidateCount :: Int
                         , candidates :: [String]
                         , tally :: (Map String Int)} deriving (Eq, Ord, Show)
data Vote = Vote [String] deriving (Eq, Ord, Read, Show)

type CandidateList = [String]
type Counter = Election -> Vote -> Election

votePlurality :: Counter
votePlurality election (Vote ballot) = election 
                                       { tally = unionWith (+) (tally election) . fromList $ zip (take 1 ballot) [1]
                                       , voteCount = (voteCount election) + 1}

voteBordaCount :: Counter
voteBordaCount election (Vote ballot) = election 
                                        { tally = unionWith (+) (tally election) $ ballotMap
                                        , voteCount = vCt + 1 }
    where ballotMap = fromList $ zip ballot [cCt, pred cCt..]
          cCt = candidateCount election
          vCt = voteCount election

                                  

makeElection :: CandidateList -> Counter -> Archive Election Vote
makeElection cs counter = Archive
                          { into = (\elec v -> counter elec v)
                          , state = Election 0 (length cs) cs Map.empty }

vote :: Archive Election Vote -> Vote -> IO (Archive Election Vote)
vote elec v = newEventToFile elec "election.arc" v

main :: IO (Archive Election Vote)
main = loadFromFile (makeElection ["A", "B", "C"] voteBordaCount) "account.arc"
