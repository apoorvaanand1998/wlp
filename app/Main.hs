module Main where

import MyLib (testParsing, z3Test)

main :: IO ()
main = do
  t <- testParsing
  z <- z3Test
  --print t
  print z