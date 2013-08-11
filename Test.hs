module Test where

import BV
import API
import Strategy

------- GENERATE
testNot  = Operators [Not] [] []
testPlus = Operators [] [Plus] []
testIf   = Operators [] [] [IfZero]
testSimple = Operators [Not] [Plus] []

------- TRAINING
testRequest = TrainingRequest (Just 10) []
testTraining = testTrain testRequest

------- GUESSING
testGuess = map (\x -> submit_guess x "0EV4CZY5rwphDeALpyJDxPrk")
              (generate (Operators {unary = [Shr16], binary = [], ternary = [], folds = []}) 3)
