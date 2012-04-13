module Main where
import CM1

fact :: Int -> CM1 String (Maybe String,Int)
fact 0 = do
  mark <- ccm
  return (mark, 1)

fact n = do
  (mark, acc) <- wcm (show n) (fact (n - 1));
  return (mark, (n * acc))

main = print $ runCM1 $ fact 5
