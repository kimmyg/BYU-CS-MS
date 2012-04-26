module K where
import CEV

data K = App1 C E K | App2 V K | Wcm1 C E K | Wcm2 V K | Clos V K | Halt

pare :: K -> K
pare (Wcm2 _ k) = k
pare k          = k

chi :: K -> [V]
chi (App1 _ _ k) = chi k
chi (App2 _ k)   = chi k
chi (Wcm1 _ _ k) = chi k
chi (Wcm2 v k)   = v:(chi k)
chi Halt         = []
