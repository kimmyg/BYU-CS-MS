module ContinuationMarkMonad where
import Control.Monad
import Data.Map
import Stack

data CM k v a = CM ((Stack (Map k v)) -> a)

runCM :: CM k v a -> a
runCM (CM m) = m [empty]

instance Monad (CM k v) where
  return x = CM (\_ -> x)
  CM m >>= f = CM (\fs -> let CM m' = f (m (empty:fs)) in m' fs)

wcm :: Ord k => k -> v -> CM k v a -> CM k v a
wcm k v (CM m) = CM (\(f:fs) -> m ((insert k v f):fs))

frameMarks :: Ord k => [k] -> Map k v -> [(k, v)]
frameMarks []     _   = []
frameMarks (k:ks) m = case Data.Map.lookup k m of
  Just v  -> (k, v):(frameMarks ks m)
  Nothing -> frameMarks ks m

ccms :: Ord k => [k] -> CM k v [[(k, v)]]
ccms ks = CM (\fs -> Prelude.filter (not . Prelude.null) (Prelude.map (\f -> frameMarks ks f) fs))

ccm :: Ord k => k -> CM k v [v]
ccm k = CM (\fs -> let CM m = ccms [k] in Prelude.map (\[(_, v)] -> v) (m fs))

getallCM :: CM k v (Stack (Map k v))
getallCM = CM (\fs -> fs)
