module Mark where

type Mark k v = (k, v)

marksSet :: (Eq k) => [(Mark k v)] -> k -> v -> [(Mark k v)]
marksSet [] key value = [(key, value)]
marksSet (m@(key', value'):ms) key value
  | key' == key = (key', value):ms
  | key' /= key = m:(marksSet ms key value)

marksGet :: (Eq k) => [(Mark k v)] -> k -> Maybe v
marksGet [] key = Nothing
marksGet (m@(key', value):ms) key
  | key' == key = Just value
  | key' /= key = marksGet ms key
