fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

fact_tr :: Integer -> Integer -> Integer
fact_tr 0 a = a
fact_tr n a = fact_tr (n - 1) (n * a)

main = print (fact_tr 1000000 1)

