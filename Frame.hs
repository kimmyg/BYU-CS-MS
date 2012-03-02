module Frame where
import Mark

data Frame = Frame [Mark] deriving Show

frameSet :: Frame -> Key -> Value -> Frame
frameSet (Frame ms) key value = Frame $ markListSet ms key value

frameGet :: Frame -> Key -> Maybe Value
frameGet (Frame ms) key = markListGet ms key

--main = print (frameGet (Frame [("name","Kimball"),("age","26")]) "age")
--main = print (frameGet (frameSet (Frame []) "name" "Kimball") "name")
