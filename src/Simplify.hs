module Simplify where

import GCLParser.GCLDatatype
import Prelude hiding (and, or)

and :: Expr -> Expr -> Expr
and (LitB False) _ = LitB False
and _ (LitB False) = LitB False
and (LitB True) e  = e
and e (LitB True)  = e
and e1 e@(BinopExpr Or e2 _) = if e1 == e2 then e1 else opAnd e1 e
and e1 e2 
    | e1 == e2         = e1
    | fst (dist e1 e2) = snd (dist e1 e2)
    | otherwise        = opAnd e1 e2
    where
        dist :: Expr -> Expr -> (Bool, Expr)
        dist (BinopExpr Or x1 y1) (BinopExpr Or x2 y2) 
            | x1 == x2 = (True, opOr x1 (opAnd y1 y2))
            | y1 == y2 = (True, opOr y1 (opAnd x1 x2))
        dist ie1 ie2   = (False, opAnd ie1 ie2)

or :: Expr -> Expr -> Expr
or (LitB True) _   = LitB True
or _ (LitB True)   = LitB True
or (LitB False) e  = e
or e (LitB False)  = e
or e1 e@(BinopExpr And e2 _) = if e1 == e2 then e1 else opOr e1 e
or e1 e2 
    | e1 == e2         = e1
    | fst (dist e1 e2) = snd (dist e1 e2)
    | otherwise        = opOr e1 e2
    where
        dist :: Expr -> Expr -> (Bool, Expr)
        dist (BinopExpr And x1 y1) (BinopExpr And x2 y2)
            | x1 == x2 = (True, 
                         opAnd x1 (opOr y1 y2))
            | y1 == y2 = (True,
                         opAnd y1 (opOr x1 x2))
        dist ie1 ie2   = (False, opOr ie1 ie2)

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

-- Boolean Algebra

