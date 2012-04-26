module V where
import C
import E

data V = V C E | L [V]
  deriving Show
