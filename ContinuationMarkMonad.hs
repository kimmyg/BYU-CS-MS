module ContinuationMarkMonad where
import Control.Monad
import Mark
import Frame
import Stack

newtype CM k v a = CM ((Stack (Frame k v)) -> a)

instance Monad (CM k v) where
  return x = CM (\_ -> x)
  r >>= f = CM (\fs -> let CM g = let CM h = r in f (h fs) in g fs)

call :: (CM k v a) -> (Stack (Frame k v)) -> a
call r fs = r ([]:fs)

wcm :: (Eq k) => k -> v -> (CM k v a) -> (Stack (Frame k v)) -> a
wcm k v r (f:fs) = r ((frameSet f k v):fs)

ccm :: (Eq k) => k -> (CM k v a) -> [v]
ccm k []     = []
ccm k (f:fs) = case (frameGet f k) of
  Nothing -> ccm k fs
  Just v  -> v:(ccm k fs)

getCM :: CM k v a
getCM = [[]]
