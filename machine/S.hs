module S where
import CEV
import K

step :: Either (C,E,K) (V,E,K) -> Either (C,E,K) (V,E,K)
step (Left (Id id,env@(E e),k)) = Right (e id,env,k)
step (Left (Abs id exp,env,k)) = Right (V (Abs id exp) env,env,k)
step (Left (App exp1 exp2,env,k)) = Left (exp1,env,App1 exp2 env k)
step (Right (v,env,App1 exp' env' k)) = Left (exp',env',App2 v k)
step (Right (v,env,App2 (V (Abs id exp) env') k)) = Left (exp,bind id v env,k)
step (Left (CNum x,env,k)) = Right (VNum x,env,k)
step (Left (Sum exp1 exp2,env,k)) = Left (exp1,env,Sum1 exp2 env k)
step (Right (v,env,Sum1 exp' env' k)) = Left (exp',env',Sum2 v k)
step (Right (VNum x,env,Sum2 (VNum y) k)) = Right (VNum (x+y),env,k)
step (Left ((Wcm exp1 exp2),env,k)) = Left (exp1,env,Wcm1 exp2 env k)
step (Right (v,env,Wcm1 exp' env' k)) = Left (exp',env',Wcm2 v (pare k))
step (Right (v,env,Wcm2 _ k)) = Right (v,env,k)
step (Left (Ccm,env,k)) = Right (L (chi k),env,k)

evalLoop :: Either (C,E,K) (V,E,K) -> V
evalLoop (Right (v,_,Halt)) = v
evalLoop s = evalLoop (step s)

eval :: C -> V
eval exp = evalLoop (Left (exp,empty,Halt))
