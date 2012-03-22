module Main where
import ContinuationMarkMonad

fact :: Int -> CM String String Int
fact 0 = return 1
fact n = wcm "fact" (show n) (do
  acc <- fact (n - 1)
  return (n * acc))

fact_tr :: Int -> Int -> CM String String Int
fact_tr 0 acc = return acc
fact_tr n acc = wcm "fact" (show n) (fact_tr (n - 1) (n * acc))

main = print $ ccm "fact" $ fact_tr 5 1
