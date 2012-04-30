module Main where
import CM1

fact :: Int -> CM1 String (Int,[String])
fact 0 = do
  ms <- ccm
  return (1,ms)

fact n = do
  (acc,ms) <- fact

liftM2 (*) (return n) (wcm (show n) (fact (n - 1)))

main = print $ runCM $ fact 5
         
--main = let x = (wcm 2 ccm) in (do
--  print $ runCM $ (wcm 1 (x >>= return))
--  print $ runCM $ (wcm 1 x))
