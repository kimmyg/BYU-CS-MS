module Main where
import Mark
import Frame
import Stack

wcm :: Stack Frame -> Key -> Value -> (Stack Frame -> Integer -> ([Value], Integer)) -> Integer -> ([Value], Integer)
wcm [] key value g = g [(frameSet (Frame []) key value)]
wcm (f:fs) key value g = g ((frameSet f key value):fs)

ccm :: Stack Frame -> Key -> [Value]
ccm [] key = []
ccm (f:fs) key = case (frameGet f key) of
    Nothing -> ccm fs key
    Just x  -> x:(ccm fs key)

fact :: Stack Frame -> Integer -> ([Value], Integer)
fact fs 0 = (ccm fs "fact", 1)
fact fs n = wcm fs "fact" (show n) (\fs -> \n -> let (ms, acc) = fact fs (n - 1) in (ms, n * acc)) n


main = print $ fact [(Frame [])] 3

{-
fact 0 = do {
	ccm "fact";
	1
}

fact n = do {
	wcm "fact" (show n);
	n * fact (n - 1)
}

fact_tr 0 acc = do {
	ccm "fact";
	acc
}

fact_tr n acc = do {
	wcm "fact" (show n);
	fact_tr (n - 1) (n * acc)
}
-}
