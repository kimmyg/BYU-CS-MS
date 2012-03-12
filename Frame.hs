module Frame where
import Mark

type Frame k v = [Mark k v]

frameSet :: (Eq k) => (Frame k v) -> k -> v -> (Frame k v)
frameSet ms key value = marksSet ms key value

frameGet :: (Eq k) => (Frame k v) -> k -> Maybe v
frameGet ms key = marksGet ms key
