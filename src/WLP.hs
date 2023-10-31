module WLP where

import qualified GCLParser.GCLDatatype as GCLD
import GCLParser.GCLDatatype (Expr, Stmt)
import Paths
import Data.Map
import qualified Simplify
import Heuristics (someHeuristic)
import Control.Monad.State (runState)
import Transformers
import Utils

wlp :: GCLD.Stmt -> GCLD.Expr -> GCLD.Expr
wlp GCLD.Skip post               = post
-- Reusing RepBy for Substitutable Expressions
-- RepBy (In this expression) (Replace this variable) (With this expression)
wlp (GCLD.Assign varname e) post = GCLD.RepBy post (GCLD.Var varname) e
wlp (GCLD.Assert e) post         = GCLD.BinopExpr GCLD.And e post
wlp (GCLD.Assume e) post         = GCLD.BinopExpr GCLD.Implication e post
wlp (GCLD.Seq s1 s2) post        = wlp s1 (wlp s2 post)
wlp (GCLD.IfThenElse {}) _       = error "IfThenElse is a Compound Statement being passed to WLP"
wlp (GCLD.While _ _) _           = error "While is a Compound Statement being passed to WLP"
wlp (GCLD.AAssign var i e) post  = GCLD.RepBy post (GCLD.Var var) (GCLD.RepBy (GCLD.Var var) i e)
-- RepBy expressions need to be changed to substituted expressions before feeding into z3
wlp (GCLD.Block _ s) post        = wlp s post
-- Assuming the block variables have been renamed to fresh variables correctly
-- Loops are a special case, rename multiple times in every iteration
-- TODO
wlp (GCLD.DrefAssign _ _) _   = error "Dref not implemented in wlp"
wlp (GCLD.TryCatch {}) _ = error "Try not implemented in wlp" -- do we go with method 1 or 2?

-- function needs clean-up...if we have time
repBy :: GCLD.Expr -> GCLD.Expr
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

treeWLP :: GCLD.Expr -> Tree GCLD.Stmt -> GCLD.Expr
treeWLP post (Tree [])      = post
treeWLP post (Tree [s])     = nodeWLP post s
treeWLP post (Tree [g, ng]) = GCLD.BinopExpr GCLD.And (nodeWLP post g) (nodeWLP post ng)
treeWLP _    _              = error "treeWLP should not be hitting this case"

nodeWLP :: GCLD.Expr -> Node GCLD.Stmt -> GCLD.Expr
nodeWLP post (Node a t) = wlp a (treeWLP post t)



-- |This function is basically "treeWLP" but with feasibility check built-in
verify :: Map String Expr -> Expr -> Tree Statement -> Expr

-- Ignore infeasible paths (unsatisfiable assumptions)
verify _ pre _ | not (satisfiable pre) = GCLD.LitB True

-- Base case
verify _ _ (Tree []) = GCLD.LitB True

-- Depending on the heuristic, either check for feasibility first or calculate wp directly
verify vars pre (Tree [Node s t])
    | someHeuristic vars pre s = let (pre', vars') = runState (sp s pre) vars in
                                 wlp (sToS s) $ verify vars' pre' t
    | otherwise                = wlp (sToS s) $ verify mempty (GCLD.LitB True) t

-- Branches
verify vars pre (Tree [Node s1 t1, Node s2 t2]) = Simplify.and (verify vars pre (Tree [Node s1 t1])) (verify vars pre (Tree [Node s2 t2]))

verify _ _ (Tree (_:_:_:_)) = error "verify should not be hitting this case"



satisfiable :: Expr -> Bool
satisfiable = error "some z3 magic"
