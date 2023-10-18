module Simplify where

import GCLParser.GCLDatatype
import Prelude hiding (and, or)

and :: Expr -> Expr -> Expr
and (LitB False) _ = LitB False
and _ (LitB False) = LitB False
and (LitB True) e  = e
and e (LitB True)  = e
and e1 e2          = BinopExpr And e1 e2

or :: Expr -> Expr -> Expr
or (LitB True) _   = LitB True
or _ (LitB True)   = LitB True
or (LitB False) e  = e
or e (LitB False)  = e
or e1 e2           = BinopExpr Or e1 e2

neg :: Expr -> Expr
neg (OpNeg e)    = e
neg (LitB True)  = LitB False
neg (LitB False) = LitB True
neg e            = OpNeg e

implies :: Expr -> Expr -> Expr
implies (LitB True)  e = e                         
implies (LitB False) _ = LitB True                 
implies _ (LitB True)  = LitB True                 
implies e (LitB False) = neg e                     
implies e1 e2          = neg e1 `or` e2