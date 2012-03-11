module ContinuationMarkMonad where
import Control.Monad
import Mark
import Frame
import Stack

instance Monad ((->) a) where
return x = \_ -> x
r >>= f = \e -> f (r e) e

call :: ((Stack Frame) -> a) -> (Stack Frame) -> a
call r fs = r ((Frame []):fs)

wcm :: Key -> Value -> ((Stack Frame) -> a) -> (Stack Frame) -> a
wcm k v r (f:fs) = r ((frameSet f k v):fs)

ccm :: (Stack Frame) -> Key -> [Value]
ccm []     k = []
ccm (f:fs) k = case (frameGet f k) of
  Nothing -> ccm fs k
  Just v  -> v:(ccm fs k)

