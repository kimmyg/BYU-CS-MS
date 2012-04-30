module CM (CM, CM.wcm, CM.ccms, CM.ccm, CM1.runCM) where
import Prelude hiding (lookup)
import Data.Map hiding (filter, map, null)
import CM1

type CM k v a = CM1 (Map k v) a

wcm :: Ord k => k -> v -> CM k v a -> CM k v a
wcm k v cm = do
  ms <- CM1.ccm
  let l = length ms in
    CM1.wcm (singleton k v) $ do
      ms' <- CM1.ccm
      let l' = length ms' in
        if l == l' then
          let (m:_) = ms in
            CM1.wcm (insert k v m) cm
        else
          CM1.wcm (singleton k v) cm

extract_single :: Ord k => [k] -> Map k v -> [(k, v)]
extract_single []     _ = []
extract_single (k:ks) m = case lookup k m of
  Nothing -> extract_single ks m
  Just v  -> (k, v):(extract_single ks m)

extract :: Ord k => [k] -> [Map k v] -> [[(k, v)]]
extract _  []     = []
extract ks (m:ms) = (extract_single ks m):(extract ks ms)

ccms :: Ord k => [k] -> CM k v [[(k, v)]]
ccms ks = CM1.ccm >>= (\ms -> return (filter (not . null) (extract ks ms)))

ccm :: Ord k => k -> CM k v [v]
ccm k = let cm = ccms [k] in cm >>= (\ms -> return (map snd (concat ms)))

