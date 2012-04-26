module Exp where

type Id = String

data Exp = Num Int | Id Id | Abs Id Exp | App Exp Exp | Wcm Exp Exp | Ccm
  deriving Show
