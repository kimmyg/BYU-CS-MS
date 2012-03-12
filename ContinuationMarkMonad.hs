module ContinuationMarkMonad where
import Control.Monad
import Mark
import Frame
import Stack

type CM k v a = (Stack (Frame [(k,v)])) -> a

instance Monad (CM k v) where
  return x = \_ -> x
  r >>= f = \e -> f (r e) e

type CM = Stack Frame

call :: (CM -> a) -> CM -> a
call r fs = r ((Frame []):fs)

wcm :: Key -> Value -> (CM -> a) -> CM -> a
wcm k v r (f:fs) = r ((frameSet f k v):fs)

ccm :: Key -> CM -> [Value]
ccm k []     = []
ccm k (f:fs) = case (frameGet f k) of
  Nothing -> ccm k fs
  Just v  -> v:(ccm k fs)

getCM :: CM
getCM = [(Frame [])]
