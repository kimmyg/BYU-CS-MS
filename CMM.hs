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
