module ContinuationMarkMonad where
import Mark
import Frame
import Stack
--import Control.Monad

{-
data CM a = CM ([Frame], a)
    deriving Show

cmReturn :: a -> CM a
cmReturn x = CM ([(Frame [])], x)

cmBind :: CM a -> (a -> CM b) -> CM b
cmBind (CM (fs, x)) g = let CM (new_fs, y) = g x in CM (new_fs++fs, y)

wcm :: CM a -> Key -> Value -> (CM a -> CM b) -> CM b
wcm (CM (f:fs, x)) key value g = g (CM ((frameSet f key value):fs, x))

ccm :: CM a -> Key -> [Value]
ccm (CM ([], _)) key = []
ccm (CM (f:fs, x)) key = case (frameGet f key) of
  Nothing -> ccm (CM (fs, x)) key
  Just cm -> cm:(ccm (CM (fs, x)) key)

instance Monad CM where
  return x = cmReturn x
  m >>= f = cmBind m f
-}

wcm :: (Stack Frame) -> Key -> Value -> ((Stack Frame) -> a) -> a
wcm [] key value g = g [(Frame [(key, value)])]
wcm (f:fs) key value g = g ((frameSet f key value):fs)

ccm :: (Stack Frame) -> Key -> [Value]
ccm [] key = []
ccm (f:fs) key = case (frameGet f key) of
  Nothing -> ccm fs key
  Just cm -> cm:(ccm fs key)

push :: (Stack Frame) -> (Stack Frame)
push fs = (Frame []):fs
