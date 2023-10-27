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
        
        dflt x y = (False, opAnd x y)
        
        lt :: Expr -> Expr -> (Bool, Expr)
        lt ie1@(BinopExpr LessThan (Var x) (LitI i)) ie2@(BinopExpr LessThan (Var y) (LitI j))
            | x == y    = (True, opLessThan (Var x) (LitI (min i j)))
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr LessThan (Var x) (LitI i)) ie2@(BinopExpr LessThanEqual (Var y) (LitI j))
            | x == y    = lt ie1 (opLessThan (Var x) (LitI (j+1)))
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr LessThanEqual (Var x) (LitI i)) ie2@(BinopExpr LessThan (Var y) (LitI j))
            | x == y    = lt (opLessThan (Var x) (LitI (i+1))) ie2
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr LessThan (Var x) (LitI i)) ie2@(BinopExpr GreaterThan (Var y) (LitI j))
            | x == y    = if j > i then (True, LitB False) else dflt ie1 ie2
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr GreaterThan (Var x) (LitI j)) ie2@(BinopExpr LessThan (Var y) (LitI i))
            | x == y    = if j > i then (True, LitB False) else dflt ie1 ie2
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr LessThan (Var x) (LitI i)) ie2@(BinopExpr GreaterThanEqual (Var y) (LitI j))
            | x == y    = lt ie1 (opGreaterThan (Var y) (LitI (j-1)))
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr GreaterThanEqual (Var x) (LitI j)) ie2@(BinopExpr LessThan (Var y) (LitI i))
            | x == y    = lt ie1 (opGreaterThan (Var y) (LitI (j-1)))
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr LessThan (Var x) (LitI i)) ie2@(BinopExpr Equal (Var y) (LitI j))
            | x == y    = if j < i then (True, opEqual (Var x) (LitI j)) else (True, LitB False)
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr Equal (Var x) (LitI j)) ie2@(BinopExpr LessThan (Var y) (LitI i))
            | x == y    = if j < i then (True, opEqual (Var x) (LitI j)) else (True, LitB False)
            | otherwise = dflt ie1 ie2
        lt ie1 ie2      = dflt ie1 ie2

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