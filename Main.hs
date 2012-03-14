module Main where
import ContinuationMarkMonad
import Mark

fact :: Int -> CM String String -> Int
fact 0 fs = 1
fact n fs = wcm "fact" (show n) (\fs -> n * (call (fact (n - 1)) fs)) fs

fact_tr :: Int -> Int -> CM String String -> Int
fact_tr 0 acc fs = acc
fact_tr n acc fs = wcm "fact" (show n) (fact_tr (n - 1) (n * acc)) fs

fact_cm :: Int -> CM String String -> [String]
fact_cm 0 fs = ccm "fact" fs
fact_cm n fs = wcm "fact" (show n) (call (fact_cm (n - 1))) fs

fact_cm_tr :: Int -> Int -> CM String String -> [String]
fact_cm_tr 0 acc fs = ccm "fact" fs
fact_cm_tr n acc fs = wcm "fact" (show n) (fact_cm_tr (n - 1) (n * acc)) fs

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

test2correct

test3 :: CM k v -> [v]
test3 fs = (do {
  wcm "test" "value" test1;
  ccm "test"
}) fs

test4 :: CM k v -> [v]
test4 fs = (do {
  wcm "fact" "present" (call (fact_cm 3))
}) fs

test5 :: CM k v -> [v]
test5 fs = (do {
  wcm "fact" "absent" (fact_cm 3)
}) fs

main = print $ test5 getCM

-}

main = print $ fact_cm_tr 5 1 getCM
