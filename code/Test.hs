module Main where
import CM

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
  (acc, ms) <- call (fact (n - 1))
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

test6 :: Bool
test6 = (==) ["v2"] $ runCM $ (wcm "key" "v1" (wcm "key" "v2" (ccm "key")))

test7 :: Bool
test7 = (==) ["v1"] $ runCM $ (wcm "k1" "v1" (wcm "k2" "v2" (ccm "k1")))

test8 :: Bool
test8 = (==) ["v2"] $ runCM $ (wcm "k1" "v1" (wcm "k2" "v2" (wcm "k1" "v3" (ccm "k2"))))

test9 :: Bool
test9 = (==) ["v3"] $ runCM $ (wcm "k1" "v1" (wcm "k2" "v2" (wcm "k1" "v3" (ccm "k1"))))

test10 :: Bool
test10 = (==) [[("k1", "v3"), ("k2", "v2")]] $ runCM $ (wcm "k1" "v1" (wcm "k2" "v2" (wcm "k1" "v3" (ccms ["k1", "k2"]))))

test11 :: Bool
test11 = (==) (2, [[("k2", "v2")], [("k1", "v1")]]) $ runCM $ do
  wcm "k1" "v1" $ call $ do
    (x, ms) <- wcm "k2" "v2" (do
      ms <- ccms ["k1", "k2"]
      return (1, ms))
    return (1+x, ms)

main = print $ [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11]

