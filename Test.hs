module Test where

import BV
import API

------- GENERATE
testNot  = Operators [Not] [] []
testPlus = Operators [] [Plus] []
testIf   = Operators [] [] [IfZero]
testSimple = Operators [Not] [Plus] []

------- TRAINING
testRequest = TrainingRequest (Just 10) []
testTraining = testTrain testRequest