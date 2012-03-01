type Key = String
type Value = String
type MarkList = [(Key, Value)]

markListSet :: MarkList -> Key -> Value -> MarkList
markListSet [] key value = [(key, value)]
markListSet (m@(key', value'):ms) key value
  | key' == key = (key', value):ms
  | key' /= key = m:(markListSet ms key value)

markListGet :: MarkList -> Key -> Maybe Value
markListGet [] key = Nothing
markListGet (m@(key', value):ms) key
  | key' == key = Just value
  | key' /= key = markListGet ms key

data Frame = Frame MarkList

frameSet :: Frame -> Key -> Value -> Frame
frameSet (Frame ms) key value = Frame $ markListSet ms key value

frameGet :: Frame -> Key -> Maybe Value
frameGet (Frame ms) key = markListGet ms key

--main = print (frameGet (Frame [("name","Kimball"),("age","26")]) "age")
main = print (frameGet (frameSet (Frame []) "name" "Kimball") "name")
