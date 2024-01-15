module Heuristics where

import Data.Map (Map)
import GCLParser.GCLDatatype (Expr)

import Paths (Statement)
import Utils (countAtoms)



type Heuristic = Map String Expr -- ^The current variable store
              -> Expr            -- ^Precondition
              -> Statement       -- ^The next statement
              -> Bool            -- ^Whether to check for feasibility



-- The heuristic that checks for feasibility only when the number of atoms of the precondition is below a threshold
atomHeuristic :: Int -> Heuristic
atomHeuristic h _ pre _ = countAtoms pre < h
