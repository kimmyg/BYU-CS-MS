module Main where
import ContinuationMarkMonad

{-
fact_tr :: Int -> CM Int -> CM Int
fact_tr 0 m = m
fact_tr n m = let CM (fs, acc) = wcm m "fact" (show n) in fact_tr (n - 1) (CM (fs, n*acc))
-}

fact :: CM Int -> CM Int
fact m@(CM (fs, 0)) = do {
	m;
	return 1;
}

fact m@(CM n) = wcm m "fact" (show n) (\m ->
  m >>= fact (return (n - 1)))


main = print (ccm (fact 5) "fact")
--main = print (ccm (fact_tr 5 (return 1)) "fact")

--wcm :: CM a -> Key -> Value -> (CM a -> CM b) -> CM b
--wcm CM (f:fs, x) key value g = g (CM ((frameSet f key value):fs, x)



{-
wcm m "fact" (show 3) (\m ->
    wcm m "fact" (show 2) (\m ->
      wcm m "fact" (show 1) (\m ->
        m >>= (return 1))))
-}

fact-tr Int -> Int -> CMM Int
fac-tr 0 acc = return acc

fact-tr n acc = do {
	wcm "fact" (show n);
	fact-tr (n - 1) (n * acc)
}

wcm :: Key -> Value -> ( CM Int -> a ) -> a
wcm key value 

wcm "fact" (show n) (\m ->
  m >>=

the continuation mark body is the expression

(a -> b) -> b

return :: a -> (a -> b) -> b
return x f = f x

wcm :: Key -> Value -> (((a -> b) -> b) -> b)
wcm key value f = create identity >>= f