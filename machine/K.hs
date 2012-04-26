module K where
import E
import V

data K = App1 E K | App2 V K | Wcm1 E K | Wcm2 V K | Halt

pare :: K -> K
pare k | (Wcm2 _ k') = k'
       | otherwise   = k

chi :: K -> [V]
chi (App1 _ k) = chi k
chi (App2 _ k) = chi k
chi (Wcm1 _ k) = chi k
chi (Wcm2 v k) = v:(chi k)
chi Halt = []

