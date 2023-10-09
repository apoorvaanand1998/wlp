module WLP where

import qualified GCLParser.GCLDatatype as GCLD
import qualified GHC.Generics as GCLD

wlp :: GCLD.Stmt -> GCLD.Expr -> GCLD.Expr
wlp GCLD.Skip post               = post
-- Reusing RepBy for Substitutable Expressions
-- RepBy (In this expression) (Replace this variable) (With this expression)
wlp (GCLD.Assign varname e) post = GCLD.RepBy post (GCLD.Var varname) e
wlp (GCLD.Assert e) post         = GCLD.BinopExpr GCLD.And e post
wlp (GCLD.Assume e) post         = GCLD.BinopExpr GCLD.Implication e post
wlp (GCLD.Seq s1 s2) post        = wlp s1 (wlp s2 post)
wlp (GCLD.IfThenElse _ _ _) _    = error "IfThenElse is a Compound Statement being passed to WLP"
wlp (GCLD.While _ _) _           = error "While is a Compound Statement being passed to WLP"
wlp (GCLD.AAssign var i e) post  = GCLD.RepBy post (GCLD.Var var) (GCLD.RepBy (GCLD.Var var) i e)
-- RepBy expressions need to be changed to substituted expressions before feeding into z3
wlp (GCLD.Block _ s) post        = wlp s post
-- Assuming the block variables have been renamed to fresh variables correctly
-- Loops are a special case, rename multiple times in every iteration
-- TODO
-- wlp (GCLD.DrefAssign _ _) post      = -- wtf
-- wlp (GCLD.TryCatch _ _ _) _ = -- do we go with method 1 or 2?

convertRepByExpr :: GCLD.Expr -> GCLD.Expr
convertRepByExpr (GCLD.RepBy _ _ (GCLD.RepBy _ _ _)) = undefined -- AAssign
--Assign
convertRepByExpr (GCLD.RepBy ve@(GCLD.Var varExpr) (GCLD.Var v) e) = if varExpr == v then e else ve
convertRepByExpr (GCLD.RepBy li@(GCLD.LitI _) _ _) = li
convertRepByExpr (GCLD.RepBy lb@(GCLD.LitB _) _ _) = lb
convertRepByExpr (GCLD.RepBy ln@GCLD.LitNull _ _)  = ln
convertRepByExpr (GCLD.RepBy (GCLD.Parens pe) v e) = convertRepByExpr (GCLD.RepBy pe v e)
convertRepByExpr (GCLD.RepBy ae@(GCLD.ArrayElem _ _) _ _) = ae -- i think
convertRepByExpr (GCLD.RepBy (GCLD.OpNeg ne) v e)  = GCLD.OpNeg (convertRepByExpr (GCLD.RepBy ne v e))
convertRepByExpr (GCLD.RepBy (GCLD.BinopExpr op be1 be2) v e) = GCLD.BinopExpr op (convertRepByExpr (GCLD.RepBy be1 v e))
                                                                                  (convertRepByExpr (GCLD.RepBy be2 v e))
convertRepByExpr (GCLD.RepBy fa@(GCLD.Forall fv fe) vr@(GCLD.Var v) e) = if fv /= v 
                                                           then GCLD.Forall fv (convertRepByExpr (GCLD.RepBy fe vr e))
                                                           else fa
convertRepByExpr (GCLD.RepBy ex@(GCLD.Exists ev ee) vr@(GCLD.Var v) e) = if ev /= v 
                                                           then GCLD.Exists ev (convertRepByExpr (GCLD.RepBy ee vr e))
                                                           else ex
                                                           
convertRepByExpr _                  = error "Non RepBy Expr trying to be substituted"

