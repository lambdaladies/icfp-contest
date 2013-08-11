module Strategy where

import System.IO (FilePath, readFile)
import System.Random.Mersenne.Pure64 (newPureMT, randomWord64)
import Data.Maybe (fromJust)
import GHC.Exts (sortWith)
import Control.Monad
import BV
import API

randomVector = randomWord64

cost_threshold = 1000000

want_to_solve p =
    not (problemSolved p) &&
    folds (problemOperators p) == [] &&
    cost p < cost_threshold &&
    has_time_left (problemTimeLeft p)

cost p = l_generate_all (problemOperators p) (problemSize p)

has_time_left (Just x) = x > 0
has_time_left Nothing  = False

sort_problems ps = sortWith cost $ filter want_to_solve ps

main = do
    problems_json <- readFile "myproblems.txt"
    --print problems_json
    problems <- return $ fromJust (decode_string problems_json :: Maybe [Problem])
    print $ length problems

    to_solve <- return $ sort_problems problems
    print $ length to_solve

    random <- newPureMT
    solve_a_problem (head to_solve) random

solve_a_problem p random = do
    print p
    try_next_candidate p all_candidates random
    where ops = problemOperators p
          n = problemSize p
          all_candidates = generate_all ops n

try_next_candidate p candidates random0 = do
    -- submit a query
    (input, random1) <- return $ randomVector random0
    eval_response <- submit_eval_request p [input]
    case evalRespStatus eval_response of
      "ok" -> do
          [output] <- return $ fromJust (evalRespOutputs eval_response)

          -- filter candidates based on eval
          remaining <- return $ filter (\c -> eval_program c input == output) candidates

          -- make a guess
          if remaining == []
          then do
              print "Failed, no candidates left :-("
          else do
              guess <- return $ head remaining
              guess_response <- submit_guess p guess
              case guessRespStatus guess_response of
                "win" -> do
                    print "Succeeded, yay!"
                "mismatch" -> do
                    -- filter candidates based on counterexample
                    [input', output', _] <- return $ fromJust (guessRespValues guess_response)
                    after_ctrexample <- return $ filter (\c -> eval_program c input == output) (tail remaining)

                    try_next_candidate p after_ctrexample random1
                _ -> do
                    print $ show_error (guessRespMessage guess_response)
      _ -> do
          print $ show_error (evalRespMessage eval_response)

show_error (Just s) = "Error: " ++ show s
show_error Nothing  = "Error: missing error message"

submit_eval_request :: Problem -> [Vector] -> IO EvalResponse
submit_eval_request p inputs = postData EvalRequest { evalReqId = problemId p, evalReqArguments = inputs } fail_eval_request evalURL

fail_eval_request :: String -> EvalResponse
fail_eval_request msg = EvalResponse { evalRespStatus = "error", evalRespOutputs = Nothing,
                                       evalRespMessage = Just msg }

submit_guess :: Problem -> Expr -> IO GuessResponse
submit_guess p guess = postData Guess { guessId = problemId p, guessProgram = guess } fail_guess guessURL

fail_guess :: String -> GuessResponse
fail_guess msg = GuessResponse { guessRespStatus = "error", guessRespValues = Nothing, guessRespLightning = Nothing,
                                 guessRespMessage = Just msg }