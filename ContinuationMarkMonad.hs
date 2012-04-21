module ContinuationMarkMonad where
import Prelude hiding (lookup)
import Control.Monad
import Data.Map hiding (filter, map, null)
import CM1

data CM k v a = CM (CM1 (Map k v) a)

instance Monad (CM k v) where
  return x = CM (return x)
  CM cm1 >>= f = CM (cm1 >>= (\x -> let (CM cm1) = f x in cm1))

wcm :: Ord k => k -> v -> CM k v a -> CM k v a
wcm k v (CM cm1) = CM $ do
  ms <- CM1.ccm
  let l = length ms in
    CM1.wcm (singleton k v) $ do
      ms' <- CM1.ccm
      let l' = length ms' in
        if l == l' then
          let (m:_) = ms in
            CM1.wcm (insert k v m) cm1
        else
          CM1.wcm (singleton k v) cm1

extract_single :: Ord k => [k] -> Map k v -> [(k, v)]
extract_single []     _ = []
extract_single (k:ks) m = case lookup k m of
  Nothing -> extract_single ks m
  Just v  -> (k, v):(extract_single ks m)

extract :: Ord k => [k] -> [Map k v] -> [[(k, v)]]
extract _  []     = []
extract ks (m:ms) = (extract_single ks m):(extract ks ms)

ccms :: Ord k => [k] -> CM k v [[(k, v)]]
ccms ks = CM (CM1.ccm >>= (\ms -> return (filter (not . null) (extract ks ms))))

ccm :: Ord k => k -> CM k v [v]
ccm k = let (CM cm1) = ccms [k] in CM (cm1 >>= (\ms -> return (map snd (concat ms))))

runCM (CM m) = runCM1 m
