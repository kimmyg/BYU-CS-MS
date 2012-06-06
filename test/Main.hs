module Main where

data A = A B | StopA
data B = B A | StopB

main = print 5
