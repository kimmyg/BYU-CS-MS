module Main where
import ContinuationMarkMonad

fact_tr :: Int -> CM Int -> CM Int
fact_tr 0 m = m
fact_tr n m = let CM (fs, acc) = wcm m "fact" (show n) in fact_tr (n - 1) (CM (fs, n*acc))

fact :: Int -> CM Int
fact 0 = do {
	return 1;
}
fact n = do {
	wcm (return n) "fact" (show n);
	let CM (fs, acc) = fact(n - 1) in CM (fs, n*acc)
}

main = print (ccm (fact 5) "fact")
--main = print (ccm (fact_tr 5 (return 1)) "fact")
