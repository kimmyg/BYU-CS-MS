module Main where
import ContinuationMarkMonad

test1 :: CM String String [String]
test1 = ccm "test"

test1correct :: [String]
test1correct = []

test2 :: CM String String [String]
test2 = wcm "test" "value" test1

test2correct :: [String]
test2correct = ["value"]

test3 :: CM String String [String]
test3 = do
  wcm "test" "value" test1
  ccm "test"

test3correct :: [String]
test3correct = []

test :: [((CM String String [String]), [String])] -> [Bool]
test [] = []
test ((m, e):ts) = (runCM m == e):(test ts)

main = print $ test [(test1, test1correct), (test2, test2correct), (test3, test3correct)]
