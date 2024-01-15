module Verification where

import qualified GCLParser.GCLDatatype as GCLD
import GCLParser.GCLDatatype (Expr)
import Paths
import Data.Map
import Simplify (memoSimplify)
import Heuristics (atomHeuristic)
import Control.Monad.State (runState)
import Transformers
import Data.Function (on)
import Metrics (Metric (..))
import Control.Monad.Writer
import Satisfiability (isFeasible)
import Utils (countAtoms)



treeWLP :: Int -> PathTree -> (Expr, [Metric])
treeWLP h tree = runWriter $ do
    expr <- verify h mempty (GCLD.LitB True) tree
    tell [Formula (countAtoms expr)]
    return expr



-- |This function is basically "treeWLP" but with feasibility check built-in
verify :: Int ->  Map String Expr -> Expr -> PathTree -> Writer [Metric] Expr

-- Ignore infeasible paths (unsatisfiable assumptions)
verify _ _ pre _ | not (isFeasible pre) = tell [Path False, Formula (countAtoms pre)] >> pure (GCLD.LitB True)

-- Base case
verify _ _ pre Terminate = tell [Path True, Formula (countAtoms pre)] >> pure (GCLD.LitB True)

-- Depending on the heuristic, either check for feasibility first or calculate wp directly
verify h vars pre (Stmt s t)
    | atomHeuristic h vars pre s = let (pre', vars') = runState (sp s pre) vars in
                                   tell [Formula (countAtoms pre)] >> wp s <$> verify h vars' (memoSimplify pre') t
    | otherwise                  = tell [Formula (countAtoms pre)] >> wp s <$> verify h mempty (GCLD.LitB True) t

-- Branches
verify h vars pre (Branch t1 t2) = memoSimplify <$> (liftA2 (GCLD.BinopExpr GCLD.And) `on` verify h vars pre) t1 t2
