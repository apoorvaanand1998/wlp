module Mutate where

import GCLParser.GCLDatatype ( Program )
import MuGCL ( mutateProgram, MutationType )
import GCLParser.Parser ( parseGCLfile )
import Options
    ( Opts(Opts, showWlp, maxDepth, invariants, simply, path, showTree,
           heuristic) )
import Paths ( programTree )
import Verification ( treeWLP )
import Metrics ( printLiveMetrics )
import Satisfiability ( counterExample )
import System.CPUTime ( getCPUTime )
import Data.Foldable ( traverse_ )
import Control.Monad ( when )
import qualified Data.Map as M
import Data.List (intercalate)

mutate :: FilePath -> IO [(MutationType, Program)]
mutate fp = do
  cp <- parseGCLfile fp
  p <- either error pure cp
  let mrs = mutateProgram p
  print $ length mrs
  return mrs

m :: FilePath -> Int -> IO ()
m fp i = do
  mrs <- mutate fp
  let options = Opts { maxDepth=100, invariants=False, simply=False, path=fp, showTree=False, heuristic=100, showWlp=False }
  start <- getCPUTime
  let (maintree, invtrees) = programTree options (snd (mrs !! i))
  let check kind pathtree = do
        putStrLn $ "Checking the " <> kind <> ":\n"
        let (theWlp, metrics) = treeWLP 10 pathtree
        printLiveMetrics metrics
        when False $ do
          putStrLn "WLP:"
          print theWlp

        case counterExample theWlp of
          Nothing -> putStrLn ("The " <> kind <> " is correct for all inputs!")
          Just output -> putStrLn $ "The " <> kind <> " is incorrect! A counterexample: " <> showMap output

  check "program" maintree
  traverse_ (check "invariant") invtrees

  end <- getCPUTime
  putStrLn $ "Computation time: " <> show ((end - start) `div` 1_000_000_000) <> "ms"

showMap :: (Show a) => M.Map String a -> String
showMap e = "{" ++ stuff e ++ "}"
  where stuff = intercalate ", " . map (\(k, v) -> k <> ": " <> show v) . M.toList
