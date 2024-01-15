module Main where

import Options (Opts(..), getOpts)
import System.CPUTime (getCPUTime)
import qualified GCLParser.Parser as GCL
import Paths (programTree)
import Control.Monad (when)
import Verification (treeWLP)
import Satisfiability (counterExample)
import Metrics (printLiveMetrics)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Foldable (traverse_)



main :: IO ()
main = do
  options@Opts { path, showTree, heuristic, showWlp } <- getOpts

  start <- getCPUTime

  parseResult <- GCL.parseGCLfile path
  program <- either error pure parseResult

  let (maintree, invtrees) = programTree options program

  let check kind pathtree = do
        putStrLn $ "Checking the " <> kind <> ":\n"
        when showTree $ print pathtree
        let (theWlp, metrics) = treeWLP heuristic pathtree
        printLiveMetrics metrics
        when showWlp $ do
          putStrLn "WLP:"
          print theWlp

        case counterExample theWlp of
          Nothing -> putStrLn ("The " <> kind <> " is correct for all inputs!")
          Just output -> putStrLn $ "The " <> kind <> " is incorrect! A counterexample: " <> showMap output

  check "program" maintree
  traverse_ (check "invariant") invtrees

  end <- getCPUTime
  putStrLn $ "Computation time: " <> show ((end - start) `div` 1_000_000_000) <> "ms"



showMap :: (Show a) => Map String a -> String
showMap e = "{" ++ stuff e ++ "}"
  where stuff = intercalate ", " . map (\(k, v) -> k <> ": " <> show v) . M.toList
