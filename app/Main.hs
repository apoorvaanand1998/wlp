module Main where

import MyLib (testParsing)

main :: IO ()
main = do
  t <- testParsing
  print t