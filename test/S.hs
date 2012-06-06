module S where
import C

step :: (C,K) -> (C,K)
step (Display x) = Display x

evalLoop :: C -> 
