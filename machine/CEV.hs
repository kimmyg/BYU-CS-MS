module CEV where

type Id = String

data E = E (Id -> V)

instance Show E where
  show e = "env"

bind :: Id -> V -> E -> E
bind id' v (E e) = E (\id -> if id == id' then v else e id)

empty :: E
empty = E (\id -> error (id ++ "not in environment"))

data V = V C E | L [V]
  deriving Show

data C = Id Id | Abs Id C | App C C | Wcm C C | Ccm
  deriving Show
