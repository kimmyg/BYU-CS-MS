module CM1 where
import Control.Monad
import Data.Maybe

-- represents a computation with annotations (unkeyed)

data CM1 m a = CM1 ([Maybe m] -> a)

instance Monad (CM1 m) where
  return x = CM1 (\_ -> x)
  (CM1 m) >>= f = CM1 (\vs -> let (CM1 m') = f (m (Nothing:vs)) in m' vs)

wcm :: m -> CM1 m a -> CM1 m a
wcm v (CM1 m) = CM1 (\(_:vs) -> m ((Just v):vs))

ucm :: (Maybe m -> m) -> CM1 m a -> CM1 m a
ucm t (CM1 m) = CM1 (\(v:vs) -> m ((Just (t v)):vs))

{--
get marks
if marks is empty
  push singleton
else
  get top mark
  push singleton

  get marks
  get top mark
  if top mark is singleton (that was just pushed)
    we're ok
  otherwise
    push insert into top mark
--}

ccm :: CM1 m [Maybe m]
--ccm = CM1 (\vs -> catMaybes vs)
ccm = CM1 id

runCM1 (CM1 m) = m (Nothing:[])
