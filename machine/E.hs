module E where
import V
import Data.Map

type E = Map String V

store :: String -> V -> E -> E
store = insert

fetch :: String -> E -> V
fetch id e = case Data.Map.lookup id e of
  (Just v) -> v
  Nothing  -> error (id ++ " not found in environment")
