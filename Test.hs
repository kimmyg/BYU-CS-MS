module Main where
import ContinuationMarkMonad

test1 :: Bool
test1 = null $ runCM $ ccm "test"

test2 :: Bool
test2 = (==) ["value"] $ runCM $ wcm "test" "value" (ccm "test")

test3 :: Bool
test3 = null $ runCM $ do
  wcm "test" "value" (ccm "test")
  ccm "test"

fact :: Int -> CM String String (Int, [String])
fact 0 = do
  ms <- ccm "fact"
  return (1, ms)
fact n = wcm "fact" (show n) (do
  (acc, ms) <- fact (n - 1)
  return (n * acc, ms))

test4 :: Bool
test4 = (==) (120, ["1", "2", "3", "4", "5"]) $ runCM $ fact 5

fact_tr :: Int -> Int -> CM String String (Int, [String])
fact_tr 0 acc = do
  ms <- ccm "fact"
  return (acc, ms)
fact_tr n acc = wcm "fact" (show n) (fact_tr (n - 1) (n * acc))

test5 :: Bool
test5 = (==) (120, ["1"]) $ runCM $ fact_tr 5 1

main = print $ [test1, test2, test3, test4, test5]
