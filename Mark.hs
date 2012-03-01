module Mark where

type Key = String
type Value = String
type Mark = (Key, Value)

markListSet :: [Mark] -> Key -> Value -> [Mark]
markListSet [] key value = [(key, value)]
markListSet (m@(key', value'):ms) key value
  | key' == key = (key', value):ms
  | key' /= key = m:(markListSet ms key value)

markListGet :: [Mark] -> Key -> Maybe Value
markListGet [] key = Nothing
markListGet (m@(key', value):ms) key
  | key' == key = Just value
  | key' /= key = markListGet ms key
