module Strategy where

import System.IO (FilePath, readFile)
import System.Random.Mersenne.Pure64 (PureMT, newPureMT, randomWord64)
import Data.Maybe (fromJust)
import GHC.Exts (sortWith)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Timer (oneShotTimer)
import Control.Concurrent.Suspend.Lifted (Delay, msDelay)
import Control.Monad
import BV
import API
import qualified Data.ByteString.Lazy.Char8 as BS


randomVector = randomWord64

cost_threshold = 100000000000

want_to_solve p =
    not (problemSolved p) &&
    -- We actually ran two instances at the end, one doing problems with 'fold' and the other without.
    --folds (problemOperators p) == [] &&
    --folds (problemOperators p) /= [] &&
    cost p < cost_threshold &&
    has_time_left (problemTimeLeft p)

cost p = l_generate_all (problemOperators p) (problemSize p)

has_time_left (Just x) = x > 30
has_time_left Nothing  = True --not started

sort_problems ps = sortWith cost $ filter want_to_solve ps

update_my_problems :: IO ()
update_my_problems = do
  problems <- getJSON problemsURL
  writeFile "myproblems2.txt" (BS.unpack problems)

main = do
    update_my_problems
    problems_json <- readFile "myproblems2.txt"
    --putStrLn problems_json
    problems <- return $ fromJust (decode_string problems_json :: Maybe [Problem])
    putStrLn $ "Number of problems: " ++ show (length problems)

    to_solve <- return $ sort_problems problems
    putStrLn $ "Number of problems to solve: " ++ show (length to_solve)

    solve_next_problem to_solve

train tReq = do
    to_solve <- testTrain tReq
    -- do not train with "big" problems
    if want_to_solve to_solve
      then solve_next_problem [to_solve]
      else putStrLn "Too big!"

solve_next_problem [] = do
    putStrLn "Done!"

solve_next_problem (p:ps) = do
    print p
    random <- newPureMT
    proceed <- try_next_candidate p all_candidates random
    if proceed
    then do
        solve_next_problem ps
    else do
        putStrLn "Stopped."
    where ops = problemOperators p
          n = problemSize p
          all_candidates = generate_all ops n

try_next_candidate :: Problem -> [Expr] -> PureMT -> IO Bool
try_next_candidate p candidates random0 = do
    delay_before_guess <- set_timer $ msDelay 5500

    --putStrLn $ "\nCandidates before eval:" ++ show (length candidates)
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
              putStrLn "Failed, no candidates left :-("
              return False
          else do
              putStrLn "Waiting..."
              wait_for delay_before_guess

              delay_before_next <- set_timer $ msDelay 5500

              --putStrLn $ "\nCandidates before guess:" ++ show (length remaining)
              guess <- return $ head remaining
              guess_response <- submit_guess p guess
              case guessRespStatus guess_response of
                "win" -> do
                    putStrLn "Succeeded, yay!"
                    return True
                "mismatch" -> do
                    -- filter candidates based on counterexample
                    [input', output', _] <- return $ fromJust (guessRespValues guess_response)
                    after_ctrexample <- return $ filter (\c -> eval_program c input' == output') (tail remaining)

                    putStrLn "Waiting..."
                    wait_for delay_before_next
                    try_next_candidate p after_ctrexample random1
                _ -> do
                -- just try again with the remaining expressions
                  putStrLn "Waiting..."
                  wait_for delay_before_next
                  try_next_candidate p (tail remaining) random1
      _ -> -- just continue and do not retry
           return True

set_timer :: Delay -> IO (MVar ())
set_timer d = do
    m <- newEmptyMVar
    oneShotTimer (do putMVar m ()) d
    return m

wait_for :: MVar () -> IO ()
wait_for m = takeMVar m

show_error (Just s) = "Error: " ++ show s
show_error Nothing  = "Error: missing error message"

submit_eval_request :: Problem -> [Vector] -> IO EvalResponse
submit_eval_request p inputs =
  postData EvalRequest { evalReqId = problemId p, evalReqArguments = inputs }
           fail_eval_request
           default_res_timeout
           evalURL

fail_eval_request :: String -> EvalResponse
fail_eval_request msg = EvalResponse { evalRespStatus = "error", evalRespOutputs = Nothing,
                                       evalRespMessage = Just msg }

submit_guess :: Problem -> Expr -> IO GuessResponse
submit_guess p guess = postData Guess { guessId = problemId p, guessProgram = guess }
                                fail_guess
                                default_res_timeout
                                guessURL

fail_guess :: String -> GuessResponse
fail_guess msg = GuessResponse { guessRespStatus = "error", guessRespValues = Nothing, guessRespLightning = Nothing,
                                 guessRespMessage = Just msg }
