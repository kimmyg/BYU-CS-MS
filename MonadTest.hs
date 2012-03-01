main = do {
	print 5;
	print 6;
	return [5]
}

{-

[5] >>= (\_ -> print 5

-}

{-

thing1 >>= (\x -> func1 x >>= (\y -> thing2 
       >>= (\_ -> func2 y (\z -> return z))))
which can be written more clearly by breaking it into several lines and 
omitting parentheses:

thing1 >>= \x ->
func1 x >>= \y ->
thing2 >>= \_ ->
func2 y >>= \z ->
return z
can be also written using the do-notation as follows:
do
  x <- thing1
  y <- func1 x
  thing2
  z <- func2 y
  return z

-}
