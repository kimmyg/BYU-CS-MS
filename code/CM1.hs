module CM1 where
import Control.Monad
import Data.Maybe

-- represents a computation with annotations (unkeyed)

data CM1 m a = CM1 ([m] -> a) | WCM1 ([m] -> a)

instance Monad (CM1 m) where
  return x = CM1 (\_ -> x)
  (CM1 m) >>= f = CM1 (\vs -> case f (m vs) of
    (CM1 m')  -> m' vs
    (WCM1 m') -> m' vs)
  (WCM1 m) >>= f = CM1 (\vs -> case f (m vs) of
    (CM1 m')  -> m' vs
    (WCM1 m') -> m' vs) 

wcm :: m -> CM1 m a -> CM1 m a
wcm v (CM1 m)  = WCM1 (\vs -> m (v:vs))
wcm v (WCM1 m) = WCM1 (\vs -> m vs)

ccm :: CM1 m [m]
ccm = CM1 id

runCM :: CM1 m a -> a
runCM (CM1 m)  = m []
runCM (WCM1 m) = m []

