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
testRequest n = TrainingRequest (Just n) []
testTraining n = testTrain (testRequest n)