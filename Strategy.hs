module Strategy where

import BV
import API

cost_threshold timeLeft = (toInteger timeLeft * 100) `min` 1000000

should_we_solve problem =
    not (problemSolved problem) &&
    l_generate (problemOperators problem) (problemSize problem) < cost_threshold (problemTimeLeft problem)
