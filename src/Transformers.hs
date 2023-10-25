module Transformers where

import Paths
import GCLParser.GCLDatatype
import Control.Monad.State
import Data.Map

-- |Weakest Precondition
wp :: Statement -> Expr -> Expr
wp = undefined

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
  arr <- gets $ findWithDefault undefined x
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
