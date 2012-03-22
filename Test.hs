module Main where
import ContinuationMarkMonad

test1 :: [String]
test1 fs = do
  ccm "test" (return 0)

test1correct :: [String]
test1correct = []

test2 :: CM String String -> [String]
test2 fs = do
  wcm "test" "value" test1

test2correct :: [String]
test2correct = ["value"]

test3 :: CM String String -> [String]
test3 fs = (do {
  wcm "test" "value" test1;
  ccm "test"
}) fs

test3correct :: [String]
test3correct = []

fact :: Int -> CM String String Int
fact 0 = return 1
fact n = wcm "fact" (show n) (do
  acc <- fact (n - 1)
  return (n * acc))

fact_tr :: Int -> Int -> CM String String Int
fact_tr 0 acc = return acc
fact_tr n acc = wcm "fact" (show n) (fact_tr (n - 1) (n * acc))

test :: [((CM String String) -> [String], [String])] -> [Bool]
test [] = []
test ((f, e):ts) = (f getCM == e):(test ts)

main = print $ test [(test1, test1correct), (test2, test2correct), (test3, test3correct)]
