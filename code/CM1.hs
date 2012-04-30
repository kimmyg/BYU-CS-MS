module CM1 where
import Control.Monad
import Data.Maybe

-- represents a computation with annotations (unkeyed)

data CM1 m a = CM1 ([Maybe m] -> a)

instance Monad (CM1 m) where
  return x = CM1 (\_ -> x)
  (CM1 m) >>= f = CM1 (\vs -> let (CM1 m') = f (m vs) in m' vs)

liftM1 :: (a -> b) -> CM1 m a -> CM1 m b
liftM1 f (CM1 m_a) = CM1 (\vs ->
  let x = m_a (Nothing:vs) in
    f x)

liftM2 :: (a -> b -> c) -> CM1 m a -> CM1 m b -> CM1 m c
liftM2 f (CM1 m_a) (CM1 m_b) = CM1 (\vs ->
  let x = m_a (Nothing:vs) in
    let y = m_b (Nothing:vs) in
      f x y)

wcm :: m -> CM1 m a -> CM1 m a
wcm v (CM1 m) = CM1 (\(_:vs) -> m ((Just v):vs))

ccm :: CM1 m [m]
ccm = CM1 catMaybes

runCM (CM1 m) = m (Nothing:[])
