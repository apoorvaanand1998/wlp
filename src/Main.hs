{-# LANGUAGE RecordWildCards #-}
module Main where

import Options (Opts(..), getOpts)
import System.CPUTime (getCPUTime)
import qualified GCLParser.Parser as GCL
import GCLParser.Parser
import Paths (programTree, limitDepth)
import Control.Monad (when)
import GCLParser.GCLDatatype (Expr(LitB, OpNeg))
import WLP (treeWLP)
import Verification (verify)
import VerificationResult (printLiveMetrics)
import Verification (isFeasible)

main :: IO ()
main = do
  Opts { path, maxDepth, showTree, heuristic, showWlp } <- getOpts

  start <- getCPUTime

  parseResult <- GCL.parseGCLfile path
  program <- either error pure parseResult

  let pathtree = limitDepth maxDepth $ programTree program
  when showTree $ print pathtree
  let (theWlp, metrics) = treeWLP heuristic pathtree
  printLiveMetrics metrics
  when showWlp $ do
    putStrLn "WLP:"
    print theWlp

  case theWlp of
    LitB True -> putStrLn "The program is correct for all inputs!"
    _ -> putStrLn "The program is incorrect! todo: print counterexample"

  end <- getCPUTime
  putStrLn $ "Computation time: " <> show ((end - start) `div` 10^9) <> "ms"
