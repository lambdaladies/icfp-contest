module Strategy where

import System.IO (FilePath, readFile)
import Control.Monad
import BV
import API

cost_threshold (Just timeLeft) = (toInteger timeLeft * 100) `min` 1000000
cost_threshold Nothing         = 1000000

want_to_solve problem =
    not (problemSolved problem) &&
    folds ops == [] &&
    l_generate ops (problemSize problem) < cost_threshold (problemTimeLeft problem)
    where ops = problemOperators problem

main = do
    json <- readFile "myproblems.txt"
    print json
