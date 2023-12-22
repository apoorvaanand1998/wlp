module Main where

import Options (Opts(..), getOpts)
import System.CPUTime (getCPUTime)
import qualified GCLParser.Parser as GCL
import Paths (programTree, limitDepth)
import Control.Monad (when)
import WLP (treeWLP)
import Verification (counterExample)
import VerificationResult (printLiveMetrics)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (intercalate)

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

  case counterExample theWlp of
    Nothing -> putStrLn "The program is correct for all inputs!"
    Just output -> putStrLn $ "The program is incorrect! A counterexample: " <> showMap output

  end <- getCPUTime
  putStrLn $ "Computation time: " <> show ((end - start) `div` 1_000_000_000) <> "ms"



showMap :: (Show a) => Map String a -> String
showMap e = "{" ++ stuff e ++ "}"
  where stuff = intercalate ", " . map (\(k, v) -> k <> ": " <> show v) . M.toList