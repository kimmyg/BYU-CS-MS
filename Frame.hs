module Frame where
import Mark

data Frame = Frame [Mark]
    deriving Show

frameSet :: Frame -> Key -> Value -> Frame
frameSet (Frame ms) key value = Frame $ marksSet ms key value

frameGet :: Frame -> Key -> Maybe Value
frameGet (Frame ms) key = marksGet ms key

--main = print (frameGet (Frame [("name","Kimball"),("age","26")]) "age")
--main = print (frameGet (frameSet (Frame []) "name" "Kimball") "name")
