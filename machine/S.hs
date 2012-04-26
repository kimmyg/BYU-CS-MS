module S where
import C
import E
import K

step :: (C,E,K) -> (C,E,K)
step (Id id,e,k) = ((fetch id e),e,k)
step (Abs,e,k) = (Clo Abs e,e,k)
step (App,e,k) = (first,e,(App1 e):k)
step (Val v,E,(App1 e' E'):k) = (e',E',(App2 v):k)
step (Val v,E,(Clo * * *):k) = (e,store id v E',k)
step (Wcm e e',E,k) = (e,E,(Wcm1 E e'):k)
step (Val v,E,(Wcm1 E' e') = (e',E',(Wcm2 v):pare(k))
step (Val v,E,(Wcm2 v'):k) = (Val v,E,k)
step (Ccm,E,k) = (chi k,E,k)
