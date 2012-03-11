module Main where
import Mark
import Frame
import Stack
import Control.Monad

{-
-- the environment monad

instance Monad ((->) e) where
  return x = \e -> x
  r >>= f = \e -> f (r e) e

ask :: e -> e
ask = id

local :: (e -> e) -> (e -> t) -> e -> t
local f c = \e -> c (f e)

-}

wcm :: Key -> Value -> ((Stack Frame) -> a) -> (Stack Frame) -> a
wcm k v g (f:fs) = g ((frameSet f k v):fs)



push :: (Stack Frame) -> (Stack Frame)
push fs = (Frame []):fs

fact :: Int -> (Stack Frame) -> [Value]
fact 0 fs  = (ccm fs "fact")
fact n fs = (wcm "fact" (show n) (fact (n - 1)) (push fs))

fact_tr :: Int -> Int -> (Stack Frame) -> [Value]
fact_tr 0 acc fs = (ccm fs "fact")
fact_tr n acc fs = wcm "fact" (show n) (fact_tr (n - 1) (n * acc)) fs


fact :: Int -> (Stack Frame) -> Int
fact 0 fs = 1
fact n fs = wcm "fact" (show n) (\fs -> n * call (fact (n - 1))) fs

fact-tr :: 

	

main = print $ fact_tr 4 1 [(Frame [])]

