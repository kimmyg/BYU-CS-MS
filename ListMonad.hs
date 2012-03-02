-- ListMonad

append :: [[a]] -> [a]
append xs = foldr (++) [] xs

listMonadReturn :: a -> [a]
listMonadReturn x = [x]

listMonadBind :: a -> (a -> [b]) -> [b]
listMonadBind xs f = append (map f xs)

instance Monad [] where

return x = listMonadReturn x
xs >>= f = listMonadBind xs f

myReturn :: a -> [a]
myReturn x = [x]

myBind :: [a] -> (a -> [b]) -> [b]
myBind xs f = append (map f xs)

main = print (map (+2) (append [[1,2,3],[4,3,2]]))
