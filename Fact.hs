fact :: Integer -> Integer
fact 0 = 1
fact n = (*) n $! fact (n - 1)

fact_tr :: Integer -> Integer -> Integer
fact_tr 0 acc = acc
fact_tr n acc = fact_tr (n - 1) $! (n * acc)

main = print (fact 10000000)
--main = print (length (concat [[x] | x <- [1..1000000]]))
