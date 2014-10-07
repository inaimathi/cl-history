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
type Counter = ((Int -> Int -> Int) -> Election -> Vote -> Election)

votePlurality :: Counter
votePlurality by election (Vote ballot) = election 
                                          { tally = unionWith by (tally election) . fromList $ zip (take 1 ballot) [1]
                                          , voteCount = (voteCount election) `by` 1}

voteBordaCount :: Counter
voteBordaCount by election (Vote ballot) = election 
                                           { tally = unionWith by (tally election) $ ballotMap
                                           , voteCount = vCt `by` 1 }
    where ballotMap = fromList $ zip ballot [cCt, pred cCt..]
          cCt = candidateCount election
          vCt = voteCount election

                                  

makeElection :: CandidateList -> Counter -> Archive Election Vote
makeElection cs counter = Archive
                          { into = (\elec v -> counter (+) elec v)
                          , outof = (\elec v -> counter (-) elec v)
                          , state = Election 0 (length cs) cs Map.empty }

vote :: Archive Election Vote -> Event Vote -> IO (Archive Election Vote)
vote elec v = newEventToFile elec "election.arc" v

main :: IO (Archive Election Vote)
main = loadFromFile (makeElection ["A", "B", "C"] voteBordaCount) "account.arc"
