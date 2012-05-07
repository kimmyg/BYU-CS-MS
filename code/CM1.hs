module CM1 where
import Control.Monad
import Data.Maybe

-- represents a computation with annotations (unkeyed)

data CM1 m a = CM1 ((Maybe m,[m]) -> a)

instance Monad (CM1 m) where
  return x = CM1 (\_ -> x)
  (CM1 m) >>= f = CM1 (\vs -> let (CM1 m') = f (m vs) in m' vs)

wcm :: m -> CM1 m a -> CM1 m a
wcm v (CM1 m) = CM1 (\(_,vs) -> m (Just v,vs))

ccm :: CM1 m [m]
ccm = CM1 (\(mv,vs) ->
  case mv of
    Just v  -> (v:vs)
    Nothing -> vs)

call :: CM1 m a -> CM1 m a
call (CM1 m) = CM1 (\(mv,vs) -> 
  case mv of
    Just v  -> m (Nothing,v:vs)
    Nothing -> m (Nothing,vs))

runCM (CM1 m) = m (Nothing,[])
