module C where

type Id = String

data C = Id Id | Abs Id C | App C C | Wcm C C | Ccm
