module Heuristics where

import GCLParser.GCLDatatype
import Paths
import Data.Map

import Utils (countAtoms)

type Heuristic = Map String Expr -- ^The current variable store
              -> Expr            -- ^Precondition
              -> Statement       -- ^The next statement
              -> Bool            -- ^Whether to check for feasibility

-- The heuristic that checks for feasibility only when the number of atoms of the precondition is below a threshold
atomHeuristic :: Int -> Heuristic
atomHeuristic h _ pre _ = countAtoms pre < h
