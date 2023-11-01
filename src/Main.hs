{-# LANGUAGE RecordWildCards #-}

module Main where

import Options (Opts(..), getOpts)

main :: IO ()
main = do
  opts@Opts{..} <- getOpts
  print opts
