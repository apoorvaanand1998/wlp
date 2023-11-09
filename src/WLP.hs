module WLP where

import qualified GCLParser.GCLDatatype as GCLD
import GCLParser.GCLDatatype (Expr)
import Paths
import Data.Map
import qualified Simplify
import Heuristics (someHeuristic)
import Control.Monad.State (runState)
import Transformers
import Data.Function (on)

wlp :: Statement -> Expr -> Expr
wlp (SAssert e)         post = GCLD.BinopExpr GCLD.And e post
wlp (SAssume e)         post = GCLD.BinopExpr GCLD.Implication e post
wlp (SAssign varname e) post = GCLD.RepBy post (GCLD.Var varname) e
wlp (SAAssign var i e)  post = GCLD.RepBy post (GCLD.Var var) (GCLD.RepBy (GCLD.Var var) i e)

-- function needs clean-up...if we have time
repBy :: Expr -> Expr
-- AAssign
-- arr[i] := e
repBy (GCLD.RepBy var@(GCLD.Var _) _ (GCLD.RepBy {})) = var
repBy (GCLD.RepBy li@(GCLD.LitI _) _ (GCLD.RepBy {})) = li
repBy (GCLD.RepBy lb@(GCLD.LitB _) _ (GCLD.RepBy {})) = lb
repBy (GCLD.RepBy ln@GCLD.LitNull  _ (GCLD.RepBy {})) = ln
repBy (GCLD.RepBy (GCLD.Parens pe) v (GCLD.RepBy _ i e)) = repBy (GCLD.RepBy pe v (GCLD.RepBy v i e))
repBy (GCLD.RepBy (GCLD.ArrayElem ae ai) v (GCLD.RepBy _ i e)) = if v == ae && i == ai -- could crash if v/ae or i/ai need to be evaluated to be equal
                                                                 then e
                                                                 else GCLD.ArrayElem ae ai -- but going with this simple approach for now as suggested
repBy (GCLD.RepBy (GCLD.OpNeg ne) v (GCLD.RepBy _ i e))  = GCLD.OpNeg (repBy (GCLD.RepBy ne v (GCLD.RepBy v i e)))
repBy (GCLD.RepBy (GCLD.BinopExpr op be1 be2) v (GCLD.RepBy _ i e)) = GCLD.BinopExpr op (repBy (GCLD.RepBy be1 v (GCLD.RepBy v i e)))
                                                                                        (repBy (GCLD.RepBy be2 v (GCLD.RepBy v i e)))
repBy (GCLD.RepBy fa@(GCLD.Forall fv fe) vr@(GCLD.Var v) (GCLD.RepBy _ i e)) = if v /= fv
                                                                               then GCLD.Forall fv (repBy (GCLD.RepBy fe vr (GCLD.RepBy vr i e)))
                                                                               else fa
repBy (GCLD.RepBy ex@(GCLD.Exists ev ee) vr@(GCLD.Var v) (GCLD.RepBy _ i e)) = if ev /= v 
                                                                               then GCLD.Exists ev (repBy (GCLD.RepBy ee vr (GCLD.RepBy vr i e)))
                                                                               else ex
repBy (GCLD.RepBy (GCLD.SizeOf se) v (GCLD.RepBy _ i e)) = GCLD.SizeOf (repBy (GCLD.RepBy se v (GCLD.RepBy v i e)))
repBy (GCLD.RepBy (GCLD.RepBy re1 re2 re3) v (GCLD.RepBy _ i e)) = repBy (GCLD.RepBy (repBy (GCLD.RepBy re1 v (GCLD.RepBy v i e)))
                                                                                     (repBy (GCLD.RepBy re2 v (GCLD.RepBy v i e)))
                                                                                     (repBy (GCLD.RepBy re3 v (GCLD.RepBy v i e))))
repBy (GCLD.RepBy (GCLD.Cond g e1 e2) v (GCLD.RepBy _ i e)) = GCLD.Cond (repBy (GCLD.RepBy g v (GCLD.RepBy v i e)))
                                                                        (repBy (GCLD.RepBy e1 v (GCLD.RepBy v i e)))
                                                                        (repBy (GCLD.RepBy e2 v (GCLD.RepBy v i e)))
--Assign
repBy (GCLD.RepBy ve@(GCLD.Var varExpr) (GCLD.Var v) e) = if varExpr == v then e else ve
repBy (GCLD.RepBy li@(GCLD.LitI _) _ _) = li
repBy (GCLD.RepBy lb@(GCLD.LitB _) _ _) = lb
repBy (GCLD.RepBy ln@GCLD.LitNull _ _)  = ln
repBy (GCLD.RepBy (GCLD.Parens pe) v e) = repBy (GCLD.RepBy pe v e)
repBy (GCLD.RepBy (GCLD.ArrayElem ae ai) v e) = GCLD.ArrayElem (repBy (GCLD.RepBy ae v e)) 
                                                               (repBy (GCLD.RepBy ai v e)) -- i think
repBy (GCLD.RepBy (GCLD.OpNeg ne) v e)  = GCLD.OpNeg (repBy (GCLD.RepBy ne v e))
repBy (GCLD.RepBy (GCLD.BinopExpr op be1 be2) v e) = GCLD.BinopExpr op (repBy (GCLD.RepBy be1 v e))
                                                                       (repBy (GCLD.RepBy be2 v e))
repBy (GCLD.RepBy fa@(GCLD.Forall fv fe) vr@(GCLD.Var v) e) = if fv /= v 
                                                              then GCLD.Forall fv (repBy (GCLD.RepBy fe vr e))
                                                              else fa
repBy (GCLD.RepBy ex@(GCLD.Exists ev ee) vr@(GCLD.Var v) e) = if ev /= v 
                                                              then GCLD.Exists ev (repBy (GCLD.RepBy ee vr e))
                                                              else ex
repBy (GCLD.RepBy (GCLD.SizeOf se) v e) = GCLD.SizeOf (repBy (GCLD.RepBy se v e))
repBy (GCLD.RepBy (GCLD.RepBy re1 re2 re3) v e) = repBy $ GCLD.RepBy (repBy (GCLD.RepBy re1 v e))
                                                                     (repBy (GCLD.RepBy re2 v e))
                                                                     (repBy (GCLD.RepBy re3 v e))
repBy (GCLD.RepBy (GCLD.Cond g e1 e2) v e) = GCLD.Cond (repBy (GCLD.RepBy g v e))
                                                       (repBy (GCLD.RepBy e1 v e))
                                                       (repBy (GCLD.RepBy e2 v e))

repBy (GCLD.RepBy (GCLD.NewStore _) _ _)    = error "NewStore not implemented in repBy"
repBy (GCLD.RepBy (GCLD.Dereference _) _ _) = error "Dereference not implemented in repBy"                                                       
repBy other                                 = other



-- |This function is basically "treeWLP" but with feasibility check built-in
verify :: Map String Expr -> Expr -> PathTree -> Expr

-- Ignore infeasible paths (unsatisfiable assumptions)
verify _ pre _ | not (satisfiable pre) = GCLD.LitB True

-- Base case
verify _ _ Terminate = GCLD.LitB True
verify _ _ Crash = GCLD.LitB False
verify _ _ Prune = GCLD.LitB True

-- Depending on the heuristic, either check for feasibility first or calculate wp directly
verify vars pre (Stmt s t)
    | someHeuristic vars pre s = let (pre', vars') = runState (sp s pre) vars in
                                 wlp s $ verify vars' pre' t
    | otherwise                = wlp s $ verify mempty (GCLD.LitB True) t

-- Branches
verify vars pre (Branch t1 t2) = (Simplify.and `on` verify vars pre) t1 t2



satisfiable :: Expr -> Bool
satisfiable = error "some z3 magic"
