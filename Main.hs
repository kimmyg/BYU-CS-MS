module Main where
import ContinuationMarkMonad
import Mark
import Frame
import Stack

fact :: Int -> (Stack Frame) -> Int
fact 0 fs = 1
fact n fs = wcm "fact" (show n) (\fs -> n * (call (fact (n - 1)) fs)) fs

fact_tr :: Int -> Int -> (Stack Frame) -> [Value]
fact_tr 0 acc fs = acc
fact_tr n acc fs = wcm "fact" (show n) (fact_tr (n - 1) (n * acc)) fs

	

main = print $ fact_tr 4 1 [(Frame [])]

