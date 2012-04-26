module E where
import C
import Data.Map

data E = E (Map Id (C,E))
  deriving Show

store :: Id -> (C,E) -> E -> E
store id v (E m) = E (insert id v m)

fetch :: Id -> E -> (C,E)
fetch id (E m) = case Data.Map.lookup id m of
  (Just v) -> v
  Nothing  -> error (id ++ " not found in environment")

empty :: E
empty = E Data.Map.empty
