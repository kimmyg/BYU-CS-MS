module Main where
import S
import CEV

main = print $ eval $ Wcm (Sum (CNum 5) (CNum 4)) Ccm
