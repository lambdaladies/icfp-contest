module Strategy where

import System.IO (FilePath, readFile)
import System.Random.Mersenne.Pure64 (newPureMT, randomWord64)
import Data.Maybe (fromJust)
import GHC.Exts (sortWith)
import Control.Monad
import BV
import API

data Outcome = Outcome { success :: Bool }

randomVector = randomWord64

cost_threshold = 1000000

want_to_solve p =
    not (problemSolved p) &&
    folds (problemOperators p) == [] &&
    cost p < cost_threshold
    -- && problemTimeLeft p > 60

cost p = l_generate_all (problemOperators p) (problemSize p)

sort_problems ps = sortWith cost $ filter want_to_solve ps

main = do
    problems_json <- readFile "myproblems.txt"
    --print problems_json
    problems <- return $ fromJust (decode_string problems_json :: Maybe [Problem])
    print $ length problems

    to_solve <- sort_problems problems
    print $ length to_solve

    solve_a_problem (head to_solve) newPureMT

solve_a_problem p random = do
    print p
    try_next_candidate all_candidates p random
    where ops = problemOperators p
          n = problemSize p
          all_candidates = generate_all ops n

try_next_candidate candidates p random0 = do
    -- submit a query
    (input,  random1) <- randomVector random0
    output <- submit_query input p

    -- filter candidates
    remaining <- return $ filter (\c -> eval_program c input == output) candidates

    -- make a guess
    if remaining == []
    then do
        print "Failed, no candidates left :-("
    else do
        guess <- return $ head remaining
        outcome <- submit_guess guess p
        if success outcome
        then do
            print "Succeeded, yay!"
        else
            try_next_candidate (tail remaining) p random1

submit_query :: Vector -> Problem -> IO ()
submit_query input p = do
    print "Query: " ++ show input ++ " " + show p
    return 0x0000000000000000

submit_guess :: Vector -> Problem -> IO ()
submit_guess guess p = do
    return Outcome { success = False }
