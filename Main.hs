module Main where
import Mark
import Frame
import Stack
import Control.Monad

instance Monad ((->) e) where
  return x = \e -> x
  r >>= f = \e -> f (r e) e

ask :: e -> e
ask = id

local :: (e -> e) -> (e -> t) -> e -> t
local f c = \e -> c (f e)

push :: (Stack Frame) -> (Stack Frame)
push fs = (Frame []):fs

ccm :: (Stack Frame) -> Key -> [Value]
ccm []     k = []
ccm (f:fs) k = case (frameGet f k) of
  Nothing -> ccm fs k
  Just v  -> v:(ccm fs k)

fact :: Int -> (Stack Frame) -> ([Value], Int)
fact 0 fs = (ccm fs "fact", 1)
fact n fs = 

{-
fact n fs = do {
	c <- get;
	local push;
	return
}
-}

main = print $ fact 0 [(Frame [])]
