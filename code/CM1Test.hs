module Main where
import CM1

fact :: Int -> CM1 String (Int,[String])
fact 0 = do
  ms <- ccm;
  return (1,ms)

fact n = do
  (acc,ms) <- wcm (show n) (fact (n - 1))
  return (n * acc,ms)

fact_tr :: Int -> Int -> CM1 String (Int,[String])
fact_tr 0 acc = do
  ms <- ccm
  return (acc,ms)

fact_tr n acc = wcm (show n) (fact_tr (n - 1) (n * acc))

main = let x = (wcm 2 ccm) in do
  putStrLn "tail recursive"
  print $ runCM $ fact_tr 5 1
  putStrLn "not tail-recursive"
  print $ runCM $ fact 5
  putStrLn "unbound"
  print $ runCM $ (wcm 1 x)
  putStrLn "bound (should be same as above)"
  print $ runCM (wcm 1 (x >>= return))

