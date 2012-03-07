module Main where
import Mark
import Frame
import Stack

data CM a = CM ([Frame], a)

instance Monad CM where
return x = cmReturn x
(>>=) m f = cmBind m f

cmReturn :: a -> CM a
cmReturn x = CM ([(Frame [])], x)

cmBind :: CM a -> (a -> CM b) -> CM b
cmBind (CM (fs, x)) g = g fs

wcm :: CM a -> Key -> Value -> CM a
wcm (CM (f:fs, x)) key value = CM ((frameSet f key value):fs, x)

ccm :: CM a -> Key -> [Value]
ccm (CM ([], _)) key = []
ccm (CM (f:fs, x)) key = case (frameGet f key) of
	Nothing -> ccm (CM (fs, x)) key
	Just cm -> cm:(ccm (CM (fs, x)) key)

fact_tr :: Int -> CM Int -> ([Value], Int)
fact_tr 0 m@(CM (_, acc)) =  m >>= \frames -> (ccm frames 

do {
	frames <- m;
	(ccm frames "fact", acc)
}

main = print 5

{-
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

--fact_tr_inner :: Stack Frame -> Integer -> Integer -> ([Value], Integer)
--fact_tr_inner fs 0 acc = (ccm fs "fact", acc)
--fact_tr_inner fs n acc = wcm fs "fact" (show n) 

--fact_tr :: Stack Frame -> Integer -> ([Value], Integer)
--fact_tr n = let fact_tr_iter n a
 
main = print $ fact [(Frame [])] 3
-}
{-
fact 0 = do {
	cms <- ccm "fact";
	return? (cms, 1)
}

fact n = do {
	wcm "fact" (show n);
	n * fact (n - 1)
}

fact_tr 0 acc = do {
	cms <- ccm "fact";
	return? (cms, acc)
}

fact_tr n acc = do {
	wcm "fact" (show n);
	fact_tr (n - 1) (n * acc)
}

constraints derived from the above:
cms :: CMM A -> Key -> [Value] in some way that works with bind
maybe we run it with a set of continuations already
a continuation mark monad is a computation with an associated set of continuation marks

runCMM :: CMM A -> Stack Frame -> something
a bind should add a continuation mark
a return is empty continuation marks with the identity function?

return :: a -> CMM a
return x = \s -> ([]

m >>= f means...



do {
	x <- return n;
	wcm "fact" (show n);
	z <- ccm "fact"



wcm ... key mark g... = \x -> g (frameSet f key mark) x

-}
