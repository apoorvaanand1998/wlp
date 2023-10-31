module Heuristics where

import GCLParser.GCLDatatype
import Paths
import Data.Map



type Heuristic = Map String Expr -- ^The current variable store
              -> Expr            -- ^Precondition
              -> Statement       -- ^The next statement
              -> Bool            -- ^Whether to check for feasibility

someHeuristic :: Heuristic
someHeuristic _vars _pre _stmt = False