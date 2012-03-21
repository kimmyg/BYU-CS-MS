module ContinuationMarkMonad where
import Control.Monad
import Data.Map
import Stack

the bind function adds or replaces 

wcm :: k -> v -> (CM -> a) -> a
wcm k v f = 

type CM k v = Stack (Map k v)

instance Monad ((->) (Stack (Map k v))) where
  return x = (\_ -> x)
  r >>= f = (\fs -> f (r fs) fs)

call :: ((Stack (Map k v)) -> a) -> (Stack (Map k v)) -> a
call r fs = r (empty:fs)

wcm :: (Ord k) => k -> v -> ((Stack (Map k v)) -> a) -> (Stack (Map k v)) -> a
wcm k v r (f:fs) = r ((insert k v f):fs)

ccm :: (Ord k) => k -> (Stack (Map k v)) -> [v]
ccm k []     = []
ccm k (f:fs) = case (Data.Map.lookup k f) of
  Nothing -> ccm k fs
  Just v  -> v:(ccm k fs)

getCM :: Stack (Map k v)
getCM = [empty]
