module Heuristics where

import Control.Monad.Reader ( Reader )
import Data.Map ( Map )
import GCLParser.GCLDatatype

import Paths ( Statement )

type Heuristic
    -- ^Precondition
    = Expr
    -- ^The next statement
    -> Statement
    -- ^The current variable store
    -> Reader (Map String Expr)
    -- ^Whether to check for feasibility
    Bool

someHeuristic :: Heuristic
someHeuristic _pre _stmt = pure False
