module Transform where
import Lambda

transform :: Term -> Term
transform (Var v)   = Abs 'Y' (Var v)
transform (Abs v e) = Abs v e
transform (App e f) = App e f
