module Stack where

type Stack a = [a]

stackEmpty :: Stack a -> Bool
stackEmpty = null

