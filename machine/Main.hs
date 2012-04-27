module Main where
import S
import CEV

main = print $ eval $ Wcm (CNum 5) (Wcm (CNum 6) Ccm)
