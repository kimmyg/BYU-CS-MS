module Exp where

type Id = String

data Exp = Id Id | Abs Id Exp | App Exp Exp | Wcm Exp Exp | Ccm
