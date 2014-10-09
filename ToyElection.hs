module ToyElection where

import History

import Data.Map (Map, unionWith, fromList, toList)
import qualified Data.Map as Map

data Election = Election { voteCount :: Int
                         , candidateCount :: Int
                         , candidates :: [String]
                         , score :: [(String, Tally, Counter)] }
instance Show Election where
    show (Election vc _ cs s) = concat (unwords cs:" (":show vc:" votes)":results)
        where results = map singleRes s
              singleRes (name, tally, _) = concat ["[ ", name, " => "
                                                  , concatMap (\(k, v) -> k ++ ":" ++ show v ++ " ") $ toList tally 
                                                  , "]"]
data Vote = Vote [String] deriving (Eq, Ord, Read, Show)

type Tally = Map String Int
type CandidateList = [String]
type Counter = (Tally -> Vote -> Tally)

applyVote :: Election -> Vote -> Election
applyVote e v = e { voteCount = succ $ voteCount e
                  , score = map tally $ score e }
    where tally (name, t, fn) = (name, fn t v, fn)

plurality :: Counter
plurality tally (Vote ballot) = unionWith (+) tally . fromList $ zip (take 1 ballot) [1]

bordaCount :: Counter
bordaCount tally (Vote ballot) = unionWith (+) tally $ ballotMap
    where ballotMap = fromList $ zip ballot [len, pred len..]
          len = length ballot

makeElection :: CandidateList -> Archive Election Vote
makeElection cs = Archive
                  { into = applyVote
                  , write = fileWrite "election.arc"
                  , state = Election 0 (length cs) cs 
                            [ ("Borda Count", Map.empty, bordaCount)
                            , ("Plurality", Map.empty, plurality) ]}

vote :: Archive Election Vote -> Vote -> IO (Archive Election Vote)
vote = newEvent

main :: IO (Archive Election Vote)
main = loadFromFile (makeElection ["A", "B", "C"]) "election.arc"
