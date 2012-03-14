module Main where
import ContinuationMarkMonad
import Mark

test1 :: CM String String -> [String]
test1 fs = (do {
  ccm "test"
}) fs

test1correct :: [String]
test1correct = []

test2 :: CM String String -> [String]
test2 fs = (do {
  wcm "test" "value" test1
}) fs

test2correct :: [String]
test2correct = ["value"]

test3 :: CM String String -> [String]
test3 fs = (do {
  wcm "test" "value" test1;
  ccm "test"
}) fs

test3correct :: [String]
test3correct = []

test :: [((CM String String) -> [String], [String])] -> [Bool]
test [] = []
test ((f, e):ts) = (f getCM == e):(test ts)

main = print $ test [(test1, test1correct), (test2, test2correct), (test3, test3correct)]
