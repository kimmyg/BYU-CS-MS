return :: Monad m => a -> m a
>>= :: Monad m => m a -> (a -> m b) -> m b

factorialCM 0 =
	ccm "n"
	1 (wrapped in some way)


	wcm "n" (show n) <some function>
		n * factorialCM (n - 1)

>>= \cms -> - a new frame or a new list?

trfactorialCM 0 =
	ccm "n"
	1 (wrapped somehow)


trfactorialcm n acc =
	if n == 0
		acc
	else
		trfactorialcm (n - 1) (n * acc)
	end


trfactorial n
	trfactorialthing n 1
	


[1] >>= \x ->
	[2] >>= \y ->
		(x,y)



fact 0
	ccm "fact"
	1

fact n
	wcm "fact" (show n)
	n * fact (n - 1)

fact-tr 0 a
	ccm "fact"
	a

fact-tr n a
	wcm "fact" (show n)
	fact-tr (n - 1) (n * a)

add a frame when there is a multiplication
