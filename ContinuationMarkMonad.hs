module ContinuationMarkMonad where
import Control.Monad
import Data.Map
import Stack

data CM k v a = CM ((Stack (Map k v)), a)
  deriving Show

instance Monad (CM k v) where
  return x = CM ([empty], x)
  (CM (ms, x)) >>= f = let (CM (ms', x')) = f x in CM (ms ++ ms', x')

wcm :: Ord k => k -> v -> CM k v a -> CM k v a
--wcm k v (CM ([],   x)) = CM ([singleton k v], x)
wcm k v (CM (m:ms, x)) = CM ((insert k v m):ms, x)

wcm :: Ord k => k -> v -> (a -> CM k v b) -> CM k v b

frameMarks :: Ord k => [k] -> Map k v -> [(k, v)]
frameMarks []     _   = []
frameMarks (k:ks) m = case Data.Map.lookup k m of
  Just v  -> (k, v):(frameMarks ks m)
  Nothing -> frameMarks ks m

ccms :: Ord k => [k] -> CM k v a -> [[(k, v)]]
ccms ks (CM (fs, _)) = Prelude.map (\f -> frameMarks ks f) fs

ccm :: Ord k => k -> CM k v a -> [v]
ccm k cm = Prelude.map (\[(_, v)] -> v) (ccms [k] cm)
