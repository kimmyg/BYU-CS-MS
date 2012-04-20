module ContinuationMarkMonad where
import Prelude hiding (lookup)
import Control.Monad
import Data.Map hiding (filter, map, null)
import CM1

data CM k v a = CM (CM1 (Map k v) a)

instance Monad (CM k v) where
  return x = CM (return x)
  CM m >>= f = CM (m >>= (\x -> let (CM m) = f x in m))

wcm :: Ord k => k -> v -> CM k v a -> CM k v a
wcm k v (CM m) = CM $ do
  ms <- CM1.ccm
  let l = length ms in
    if l == 0 then
      CM1.wcm (singleton k v) m
    else
      let (m':_) = ms in
        let m'' = insert k v m' in
          CM1.wcm m'' $ do
            ms <- CM1.ccm
            let l' = length ms in
              if l == l' then
                CM1.wcm m'' m
              else
                CM1.wcm (singleton k v) m

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
ccm k = let (CM m) = ccms [k] in CM (m >>= (\ms -> return (map (\(_, v) -> v) (concat ms))))

runCM (CM m) = runCM1 m
