module Main where
import Frame
import Stack

type Continuation a = (Stack Frame, a)

--cAdd :: (Num a) => Stack[Frame] -> a -> a -> Continuation a
--cAdd

fac :: Stack Frame -> Int -> Continuation Int
fac frames 0 = (frames, 1)
fac frames n = n * (fac ([]:frames) (n - 1))

facacc :: Stack Frame -> Int -> Int -> Continuation Int
facacc frames 0 acc = (frames, acc)
facacc frames n acc = facacc frames (n - 1) (n * acc) 

factr :: Stack Frame -> Int -> Continuation Int
factr frames n = facacc frames n 1

--main = print $ frameSet (frameSet (Frame []) "name" "Kimball") "name" "Rosalie"
main = print $ [Frame []]
