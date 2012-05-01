module CM1 where
import Control.Monad
import Data.Maybe

-- represents a computation with annotations (unkeyed)

data CM1 m a = CM1 ([Maybe m] -> a) | CM1TP ([Maybe m] -> a)

instance Monad (CM1 m) where
  return x = CM1TP (\_ -> x)
  (CM1 m) >>= f = case f (m (Nothing:[])) of
    (CM1 _)   -> CM1 (\vs -> let (CM1 m') = f (m (Nothing:vs)) in m' vs)
    (CM1TP _) -> CM1TP (\vs -> let (CM1TP m') = f (m vs) in m' vs)
  (CM1TP m) >>= f = case f (m (Nothing:[])) of
    (CM1 _)   -> CM1 (\vs -> let (CM1 m') = f (m (Nothing:vs)) in m' vs)
    (CM1TP _) -> CM1TP (\vs -> let (CM1TP m') = f (m vs) in m' vs)

wcm :: m -> CM1 m a -> CM1 m a
wcm v (CM1 m)   = CM1 (\(_:vs) -> m ((Just v):vs))
wcm v (CM1TP m) = CM1TP (\(_:vs) -> m ((Just v):vs))

ccm :: CM1 m [m]
ccm = CM1 catMaybes

runCM :: CM1 m a -> a
runCM (CM1 m)   = m (Nothing:[])
runCM (CM1TP m) = m (Nothing:[])
