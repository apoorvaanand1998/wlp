module WLP where

import qualified GCLParser.GCLDatatype as GCLD

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
-- TODO
-- wlp (GCLD.AAsign _ _ _) post     = Repby --something
-- wlp (GCLD.DrefAssign _ _) post      = -- wtf
-- wlp (GCLD.Block _ _) _ = -- convert to something else and then call WLP on that?
-- wlp (GCLD.TryCatch _ _ _) _ = -- do we go with method 1 or 2?