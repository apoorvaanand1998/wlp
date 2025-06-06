module Transformers where

import Paths
import GCLParser.GCLDatatype
import Control.Monad.State
import Data.Map
import qualified GCLParser.GCLDatatype as GCLD
import Simplify (simplifyAll, memoSimplify)

-- |Weakest Precondition
wp :: Statement -> Expr -> Expr
wp (SAssert e)         post = memoSimplify (GCLD.opAnd e post)
wp (SAssume e)         post = memoSimplify $ GCLD.BinopExpr GCLD.Implication e post
wp (SAssign varname e) post = repBy $ GCLD.RepBy post (GCLD.Var varname) e
wp (SAAssign var i e)  post = repBy $ GCLD.RepBy post (GCLD.Var var) (GCLD.RepBy (GCLD.Var var) i e)

-- |Strongest Postcondition
sp :: Statement -> Expr -> State (Map String Expr) Expr
sp (SAssert e) p = BinopExpr And p <$> eval e
sp (SAssume e) p = BinopExpr And p <$> eval e
sp (SAssign x e) p = do
  e' <- eval e
  modify (insert x e')
  pure p
sp (SAAssign x i e) p = do
  e' <- eval e
  arr <- gets $ findWithDefault (ArrayElem (Var x) i) x
  modify (insert x (RepBy arr i e'))
  pure p

eval :: Expr -> State (Map String Expr) Expr
eval (Var x) = gets $ findWithDefault (Var x) x

eval (Parens e)         = Parens      <$> eval e
eval (ArrayElem a i)    = ArrayElem   <$> eval a <*> eval i
eval (OpNeg e)          = OpNeg       <$> eval e
eval (BinopExpr o a b)  = BinopExpr o <$> eval a <*> eval b
eval (Forall x e)       = Forall x    <$> eval e
eval (Exists x e)       = Exists x    <$> eval e
eval (SizeOf e)         = SizeOf      <$> eval e
eval (RepBy x i e)      = RepBy       <$> eval x <*> eval i <*> eval e
eval (Cond g t f)       = Cond        <$> eval g <*> eval t <*> eval f
eval (NewStore e)       = NewStore    <$> eval e

eval (LitI i)           = pure (LitI i)
eval (LitB b)           = pure (LitB b)
eval LitNull            = pure LitNull
eval (Dereference x)    = pure $ Dereference x

(|>) :: Statement -> Expr -> Expr
(|>) = wp

(<|) :: Expr -> Statement -> State (Map String Expr) Expr
(<|) = flip sp

repBy :: Expr -> Expr
repBy (GCLD.RepBy var@(GCLD.Var _) _ (GCLD.RepBy {})) = var
repBy (GCLD.RepBy li@(GCLD.LitI _) _ (GCLD.RepBy {})) = li
repBy (GCLD.RepBy lb@(GCLD.LitB _) _ (GCLD.RepBy {})) = lb
repBy (GCLD.RepBy ln@GCLD.LitNull  _ (GCLD.RepBy {})) = ln
repBy (GCLD.RepBy (GCLD.Parens pe) v (GCLD.RepBy _ i e)) = simplifyAll $ repBy (GCLD.RepBy pe v (GCLD.RepBy v i e))
repBy (GCLD.RepBy (GCLD.ArrayElem ae ai) v (GCLD.RepBy _ i e)) = if v == ae && i == ai
                                                                 then e
                                                                 else GCLD.ArrayElem ae ai
repBy (GCLD.RepBy (GCLD.OpNeg ne) v (GCLD.RepBy _ i e))  = GCLD.OpNeg (simplifyAll (repBy (GCLD.RepBy ne v (GCLD.RepBy v i e))))
repBy (GCLD.RepBy (GCLD.BinopExpr op be1 be2) v (GCLD.RepBy _ i e)) = GCLD.BinopExpr op (simplifyAll (repBy (GCLD.RepBy be1 v (GCLD.RepBy v i e))))
                                                                                        (simplifyAll (repBy (GCLD.RepBy be2 v (GCLD.RepBy v i e))))
repBy (GCLD.RepBy fa@(GCLD.Forall fv fe) vr@(GCLD.Var v) (GCLD.RepBy _ i e)) = if v /= fv
                                                                               then GCLD.Forall fv (simplifyAll (repBy (GCLD.RepBy fe vr (GCLD.RepBy vr i e))))
                                                                               else fa
repBy (GCLD.RepBy ex@(GCLD.Exists ev ee) vr@(GCLD.Var v) (GCLD.RepBy _ i e)) = if ev /= v 
                                                                               then GCLD.Exists ev (simplifyAll (repBy (GCLD.RepBy ee vr (GCLD.RepBy vr i e))))
                                                                               else ex
repBy (GCLD.RepBy (GCLD.SizeOf se) v (GCLD.RepBy _ i e)) = GCLD.SizeOf (repBy (GCLD.RepBy se v (GCLD.RepBy v i e)))
repBy (GCLD.RepBy (GCLD.RepBy re1 re2 re3) v (GCLD.RepBy _ i e)) = repBy (GCLD.RepBy (simplifyAll (repBy (GCLD.RepBy re1 v (GCLD.RepBy v i e))))
                                                                                     (simplifyAll (repBy (GCLD.RepBy re2 v (GCLD.RepBy v i e))))
                                                                                     (simplifyAll (repBy (GCLD.RepBy re3 v (GCLD.RepBy v i e)))))
repBy (GCLD.RepBy (GCLD.Cond g e1 e2) v (GCLD.RepBy _ i e)) = GCLD.Cond (simplifyAll (repBy (GCLD.RepBy g v (GCLD.RepBy v i e))))
                                                                        (simplifyAll (repBy (GCLD.RepBy e1 v (GCLD.RepBy v i e))))
                                                                        (simplifyAll (repBy (GCLD.RepBy e2 v (GCLD.RepBy v i e))))
repBy (GCLD.RepBy ve@(GCLD.Var varExpr) (GCLD.Var v) e) = if varExpr == v then e else ve
repBy (GCLD.RepBy li@(GCLD.LitI _) _ _) = li
repBy (GCLD.RepBy lb@(GCLD.LitB _) _ _) = lb
repBy (GCLD.RepBy ln@GCLD.LitNull _ _)  = ln
repBy (GCLD.RepBy (GCLD.Parens pe) v e) = simplifyAll (repBy (GCLD.RepBy pe v e))
repBy (GCLD.RepBy (GCLD.ArrayElem ae ai) v e) = GCLD.ArrayElem (simplifyAll (repBy (GCLD.RepBy ae v e)))
                                                               (simplifyAll (repBy (GCLD.RepBy ai v e)))
repBy (GCLD.RepBy (GCLD.OpNeg ne) v e)  = GCLD.OpNeg (simplifyAll (repBy (GCLD.RepBy ne v e)))
repBy (GCLD.RepBy (GCLD.BinopExpr op be1 be2) v e) = GCLD.BinopExpr op (simplifyAll (repBy (GCLD.RepBy be1 v e)))
                                                                       (simplifyAll (repBy (GCLD.RepBy be2 v e)))
repBy (GCLD.RepBy fa@(GCLD.Forall fv fe) vr@(GCLD.Var v) e) = if fv /= v 
                                                              then GCLD.Forall fv (simplifyAll (repBy (GCLD.RepBy fe vr e)))
                                                              else fa
repBy (GCLD.RepBy ex@(GCLD.Exists ev ee) vr@(GCLD.Var v) e) = if ev /= v 
                                                              then GCLD.Exists ev (simplifyAll (repBy (GCLD.RepBy ee vr e)))
                                                              else ex
repBy (GCLD.RepBy (GCLD.SizeOf se) v e) = GCLD.SizeOf (repBy (GCLD.RepBy se v e))
repBy (GCLD.RepBy (GCLD.RepBy re1 re2 re3) v e) = repBy $ GCLD.RepBy (simplifyAll (repBy (GCLD.RepBy re1 v e)))
                                                                     (simplifyAll (repBy (GCLD.RepBy re2 v e)))
                                                                     (simplifyAll (repBy (GCLD.RepBy re3 v e)))
repBy (GCLD.RepBy (GCLD.Cond g e1 e2) v e) = GCLD.Cond (simplifyAll (repBy (GCLD.RepBy g v e)))
                                                       (simplifyAll (repBy (GCLD.RepBy e1 v e)))
                                                       (simplifyAll (repBy (GCLD.RepBy e2 v e)))

repBy (GCLD.RepBy (GCLD.NewStore _) _ _)    = error "NewStore not implemented in repBy"
repBy (GCLD.RepBy (GCLD.Dereference _) _ _) = error "Dereference not implemented in repBy"                                                       
repBy other                                 = other
