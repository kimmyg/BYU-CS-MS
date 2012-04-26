module S where
import C
import E
import K
import V

step :: (Either C V,E,K) -> (Either C V,E,K)
step (Left (Id id),env,k) = (Right (fetch id env),env,k)
step (Left (Abs id exp),env,k) = (Right (V exp env),env,k)
step (Left (App exp1 exp2),env,k) = (Left exp1,env,App1 exp2 env k)
--step (Right v,env,App1 exp' env' k) = (Left exp',env',App2 v k)
--step (Right v,env,Clos (V exp env') k) = (Left exp,store id v env,k)
step (Left (Wcm exp1 exp2),env,k) = (Left exp1,env,Wcm1 exp2 env k)
step (Right v,env,Wcm1 exp' env' k) = (Left exp',env',Wcm2 v (pare k))
step (Right v,env,Wcm2 _ k) = (Right v,env,k)
step (Left Ccm,env,k) = (Right (L (chi k)),env,k)

evalLoop :: (Either C V,E,K) -> V
evalLoop (Right v,_,Halt) = v
evalLoop s = evalLoop (step s)

eval :: C -> V
eval exp = evalLoop (Left exp,empty,Halt)
