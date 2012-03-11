module ContinuationMarkMonad where
import Control.Monad
import Mark
import Frame
import Stack

instance Monad ((->) a) where
  return x = \_ -> x
  r >>= f = \e -> f (r e) e

type CM = Stack Frame

call :: (CM -> a) -> CM -> a
call r fs = r ((Frame []):fs)

wcm :: Key -> Value -> (CM -> a) -> CM -> a
wcm k v r (f:fs) = r ((frameSet f k v):fs)

ccm :: CM -> Key -> [Value]
ccm []     k = []
ccm (f:fs) k = case (frameGet f k) of
  Nothing -> ccm fs k
  Just v  -> v:(ccm fs k)

getCM :: CM
getCM = [(Frame [])]