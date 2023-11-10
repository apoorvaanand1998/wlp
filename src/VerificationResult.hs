module VerificationResult where

import GCLParser.GCLDatatype (Expr)
import System.Console.ANSI
import Control.Concurrent
import Data.List (intercalate)

-- Metrics to be collected during verification
data Metric = Path { feasible :: Bool } -- for the total number of (infeasible) paths
            | Formula { size :: Int }   -- for the total formula size

printLiveMetrics :: [Metric] -> IO ()
printLiveMetrics ms = print (every 100 intermediates <> [last intermediates]) -- todo: tweak number if number isn't updated often enough
    where intermediates = scanl f (IntermediateResult 0 0 0) ms
          f (IntermediateResult i j k) (Path True) = IntermediateResult (i+1) j k
          f (IntermediateResult i j k) (Path False) = IntermediateResult (i+1) (j+1) k
          f (IntermediateResult i j k) (Formula s) = IntermediateResult i j (k+s)

data IntermediateResult = IntermediateResult
    { inspectedPaths :: Int
    , infeasiblePaths :: Int
    , totalFormulaSize :: Int
    }

instance Show IntermediateResult where
    show (IntermediateResult i j k) =
        "Number of inspected paths:\t" ++ show i ++ "\n"
        <> "Number of infeasible paths:\t" ++ show j ++ "\n"
        <> "Total formula size:\t" ++ show k ++ "\n"

    showList [] = showString "no results"
    showList [x] = showString (show x)
    showList (x:xs) = showString (show x)
                    . shows (length (show $ head xs))
                    . showString (cursorUpLineCode 3 <> clearFromCursorToScreenEndCode)
                    . showList xs

every :: Int -> [a] -> [a]
every n (x:xs) = x : every n (drop (n-1) xs)
every _ [] = []
