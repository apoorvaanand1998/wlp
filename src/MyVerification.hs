module MyVerification where
import Paths
import GCLParser.GCLDatatype
import Control.Monad.State
import Data.Map
import Transformers
import Simplify
import Control.Monad.Reader

-- |An alternative alternative tree
data PathTree = Stop
              | Stmt Statement PathTree
              | Branch PathTree PathTree

data Result = Valid | Invalid

heuristic :: Map String Expr -> Expr -> Statement -> Bool
heuristic _ _ _ = False



-- verify :: PathTree -> Map String Expr -> Expr
-- verify Stop _ = LitB True

-- -- Ignore infeasible paths (unsatisfiable assumptions)
-- verify (Stmt (SAssume p) _) _ | not (satisfiable p) = LitB True

-- -- Base case
-- verify (Stmt (SAssume _) Stop) _ = LitB True

-- -- Branches
-- verify (Stmt (SAssume p) (Branch t1 t2)) m = verify (Stmt (SAssume p) t1) m `Simplify.and` verify (Stmt (SAssume p) t2) m

-- -- Depending on the heuristic, either check for feasibility first or calculate wp directly
-- verify (Stmt (SAssume p) (Stmt s tree)) m
--     | heuristic s p m = let (e', m') = runState (sp s p) m in
--                         wp s $ verify (Stmt (SAssume e') tree) m'
--     | otherwise       = wp (SAssume p) $ verify (Stmt s tree) m

-- -- Add a precondition (if it's not already there)
-- verify tree m = verify (Stmt (SAssume (LitB True)) tree) m



verify :: Map String Expr -> Expr -> PathTree -> Expr
-- Base case
verify _ _ Stop = LitB True

-- Ignore infeasible paths (unsatisfiable assumptions)
verify _ p _ | not (satisfiable p) = LitB True

-- Branches
verify m p (Branch t1 t2) = verify m p t1 `Simplify.and` verify m p t2

-- Depending on the heuristic, either check for feasibility first or calculate wp directly
verify m p (Stmt s tree)
    | heuristic m p s = let (p', m') = runState (sp s p) m in
                        wp s $ verify m' p' tree
    | otherwise       = wp s $ verify mempty (LitB True) tree





satisfiable :: Expr -> Bool
satisfiable = undefined -- some z3 magic
