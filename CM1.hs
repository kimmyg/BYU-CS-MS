module CM1 where
import Control.Monad

-- represents a computation with a single annotation

data CM1 m a = CM1 (Maybe m -> a)

instance Monad (CM1 m) where
  return x = CM1 (\_ -> x)
  (CM1 m) >>= f = CM1 (\v -> let (CM1 m') = f (m v) in m' v)

wcm :: m -> CM1 m a -> CM1 m a
wcm v (CM1 m) = CM1 (\_ -> m (Just v))

ccm :: CM1 m (Maybe m)
ccm = CM1 (\v -> v)

runCM1 (CM1 m) = m Nothing
