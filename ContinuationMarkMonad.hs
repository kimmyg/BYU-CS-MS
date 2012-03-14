module ContinuationMarkMonad where
import Control.Monad
import Mark
import Frame
import Stack

type CM k v = Stack (Frame k v)

instance Monad ((->) [[(k, v)]]) where
  return x = (\_ -> x)
  r >>= f = (\fs -> f (r fs) fs)

call :: ((Stack (Frame k v)) -> a) -> (Stack (Frame k v)) -> a
call r fs = r ([]:fs)

wcm :: (Eq k) => k -> v -> ((Stack (Frame k v)) -> a) -> (Stack (Frame k v)) -> a
wcm k v r (f:fs) = r ((frameSet f k v):fs)

ccm :: (Eq k) => k -> (Stack (Frame k v)) -> [v]
ccm k []     = []
ccm k (f:fs) = case (frameGet f k) of
  Nothing -> ccm k fs
  Just v  -> v:(ccm k fs)

getCM :: Stack (Frame k v)
getCM = [[]]
