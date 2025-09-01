module Main (main) where

import StableMarriage
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [testCase "Preference test #1" $
    (isStableMatching prefs1 . deferredAcceptanceAlg) prefs1 @?= True,
   testCase "Preference test (women propose)" $
    (isStableMatching prefs1 . deferredAcceptanceAlgWomen) prefs1 @?= True,
   testCase "Preference test #2" $
    (isStableMatching prefs2 . deferredAcceptanceAlg) prefs2 @?= True,
   testCase "Preference test #3" $
    (isStableMatching prefs3 . deferredAcceptanceAlg) prefs3 @?= True,
   testCase "Preference test #4" $
    (isStableMatching prefs4 . deferredAcceptanceAlg) prefs4 @?= True]


-- * Test preference relations

prefs1 :: PrefProfile String String
prefs1 = listToPrefProfile
  ([("m1", ["w1", "w3", "w4", "w2"]), ("m2", ["w1", "w3", "w2", "w4"]),
    ("m3", ["w2", "w1", "w3", "w4"]), ("m4" , ["w2", "w4" , "w1", "w3"])],
   [("w1", ["m4", "m3", "m1", "m2"]), ("w2", ["m2", "m4", "m1", "m3"]),
    ("w3", ["m4", "m1", "m2", "m3"]), ("w4", ["m3", "m2", "m1", "m4"])])

prefs2 :: PrefProfile Int Int
prefs2 = listToPrefProfile
  ([(0, [7,5,6,4]), (1, [5,4,6,7]),
    (2, [4, 5, 6, 7]), (3, [4, 5, 6, 7])],
   [(4, [0, 1, 2, 3]), (5, [0, 1, 2, 3]),
    (6, [0, 1, 2, 3]), (7, [0, 1, 2, 3])])

prefs3 :: PrefProfile String String
prefs3 = listToPrefProfile
  ([("m1", ["w1", "w2", "w3"]), ("m2", ["w2", "w1", "w3"]), ("m3", ["w1", "w2", "w3"])],
   [("w1", ["m2", "m3", "m1"]), ("w2", ["m1", "m3", "m2"]), ("w3", ["m1", "m2", "m3"])])

prefs4 :: PrefProfile String String
prefs4 = listToPrefProfile
  ([("m1", ["w1", "w2", "w3", "w4"]), ("m2", ["w1", "w4", "w3", "w2"]),
    ("m3", ["w2", "w1", "w3", "w4"]), ("m4" , ["w4", "w2" , "w3", "w1"])],
   [("w1", ["m4", "m3", "m1", "m2"]), ("w2", ["m2", "m4", "m1", "m3"]),
    ("w3", ["m4", "m1", "m2", "m3"]), ("w4", ["m3", "m2", "m1", "m4"])])


