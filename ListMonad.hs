-- ListMonad

fold :: (b -> a -> a) -> a -> [b] -> a
fold f acc [] = acc
fold f acc (x:xs) = f x (fold f acc xs)

append :: [[a]] -> [a]
append xs = fold (++) [] xs

myReturn :: a -> [a]
myReturn x = [x]

myBind :: [a] -> (a -> [b]) -> [b]
myBind xs f = append (map f xs)

main = print (map (+2) (append [[1,2,3],[4,3,2]]))
