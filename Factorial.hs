fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

facacc :: Int -> Int -> Int
facacc n acc
  | n == 0    = acc
  | otherwise = facacc (n - 1) (acc * n)

factr :: Int -> Int
factr n = facacc n 1

main = print $ factr 6
