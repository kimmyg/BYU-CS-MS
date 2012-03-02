module Mark where

type Key = String
type Value = String
type Mark = (Key, Value)

marksSet :: [Mark] -> Key -> Value -> [Mark]
marksSet [] key value = [(key, value)]
marksSet (m@(key', value'):ms) key value
  | key' == key = (key', value):ms
  | key' /= key = m:(marksSet ms key value)

marksGet :: [Mark] -> Key -> Maybe Value
marksGet [] key = Nothing
marksGet (m@(key', value):ms) key
  | key' == key = Just value
  | key' /= key = marksGet ms key
