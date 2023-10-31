module Heuristics where

import GCLParser.GCLDatatype
import Paths
import Control.Monad.Reader
import Data.Map



type Heuristic = Expr                     -- ^Precondition
              -> Statement                -- ^The next statement
              -> Reader (Map String Expr) -- ^The current variable store
                 Bool                     -- ^Whether to check for feasibility

someHeuristic :: Heuristic
someHeuristic _pre _stmt = pure False