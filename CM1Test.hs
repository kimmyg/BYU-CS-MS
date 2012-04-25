module Main where
import CM1

fact :: Int -> CM1 String ([String], Int)
fact 0 = do
  marks <- ccm
  return (marks, 1)

fact n = do
  (marks, acc) <- wcm (show n) (fact (n - 1));
  return (marks, (n * acc))
           
main = let x = (wcm 2 ccm) in print $ runCM $ (wcm 1 (x >>= return))
