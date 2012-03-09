module Main where
import Mark
import Frame
import Stack
import ContinuationMarkMonad

fact :: (Stack Frame) -> Int -> ([Value], Int)
fact frames 0 = ((ccm frames "fact"), 1)
fact frames n = wcm frames "fact" (show n) (\frames -> let (ms, acc) = (fact (push frames) (n - 1)) in (ms, n * acc))

fact_tr :: (Stack Frame) -> Int -> Int -> ([Value], Int)
fact_tr frames 0 acc = ((ccm frames "fact"), acc)
fact_tr frames n acc = wcm frames "fact" (show n) (\frames -> fact_tr frames (n - 1) (n * acc))

main = print $ let (ms, v) = (fact_tr [] 5 1) in v
