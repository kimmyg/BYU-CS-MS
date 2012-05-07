module Main where
import CM

fact :: Int -> CM String String (Int, [String])
fact 0 = do
  ms <- ccm "fact"
  return (1, ms)

fact n = wcm "fact" (show n) $ do
  (acc, ms) <- call (fact (n - 1))
  return ((n * acc), ms)

fact_tr :: Int -> Int -> CM String String (Int, [String])
fact_tr 0 acc = do
  ms <- ccm "fact"
  return (acc, ms)

fact_tr n acc = wcm "fact" (show n) (fact_tr (n - 1) (n * acc))

main = do
  putStrLn "fact 3";
  print $ runCM $ fact 3;
  putStrLn "fact_tr 3 1";
  print $ runCM $ fact_tr 3 1
