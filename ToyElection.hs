module ToyElection where

import History

import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map, unionWith, fromList, toList)
import qualified Data.Map as Map

data Election = Election { voteCount :: Int
                         , candidateCount :: Int
                         , candidates :: [String]
                         , ballots :: [Vote]
                         , votingSystems :: [(String, Counter)] }
instance Show Election where
    show (Election 0 _ cs _ _) = unwords cs ++ " (0 votes) []"
    show (Election vc _ cs bs s) = concat (unwords cs:" (":show vc:" votes) ":results)
        where results = map singleRes s
              singleRes (name, fn) = concat ["[ ", name, " => "
                                            , fst . head $ resultsBy bs fn
                                            , " ]"]
data Vote = Vote [String] deriving (Eq, Ord, Read, Show)

type Tally = Map String Int
type CandidateList = [String]
type Counter = (Tally -> Vote -> Tally)

resultsBy :: [Vote] -> Counter -> [(String, Int)]
resultsBy vs fn = sorted . toList $ foldl fn Map.empty vs
    where sorted = sortBy (flip compare `on` snd)

applyVote :: Election -> Vote -> Election
applyVote e v = e { voteCount = succ $ voteCount e
                  , ballots = v : (ballots e) }

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
                  , state = Election 0 (length cs) cs []
                            [ ("Borda Count", bordaCount)
                            , ("Plurality", plurality) ]}

vote :: Archive Election Vote -> Vote -> IO (Archive Election Vote)
vote = newEvent

main :: IO (Archive Election Vote)
main = loadFromFile (makeElection ["A", "B", "C"]) "election.arc"
