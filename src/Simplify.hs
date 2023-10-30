{-# OPTIONS_GHC -Wno-unused-matches #-}
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
    | fst (lt e1 e2) = snd (lt e1 e2)
    | fst (gt e1 e2) = snd (gt e1 e2)
    | fst (lte e1 e2) = snd (lte e1 e2)
    | fst (gte e1 e2) = snd (gte e1 e2)
    | fst (eql e1 e2) = snd (eql e1 e2)
    | otherwise       = opAnd e1 e2
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

        gt :: Expr -> Expr -> (Bool, Expr)
        gt ie1@(BinopExpr GreaterThan (Var x) (LitI i)) ie2@(BinopExpr GreaterThan (Var y) (LitI j))
            | x == y    = (True, opGreaterThan (Var x) (LitI (max i j)))
            | otherwise = dflt ie1 ie2
        gt ie1@(BinopExpr GreaterThan (Var x) (LitI i)) ie2@(BinopExpr LessThanEqual (Var y) (LitI j))
            | x == y    = lt ie1 (BinopExpr LessThan (Var y) (LitI (j+1)))
            | otherwise = dflt ie1 ie2
        gt ie1@(BinopExpr LessThanEqual (Var x) (LitI j)) ie2@(BinopExpr GreaterThan (Var y) (LitI i))
            | x == y    = lt ie1 (BinopExpr LessThan (Var y) (LitI (j+1)))
            | otherwise = dflt ie1 ie2
        gt ie1@(BinopExpr GreaterThan (Var x) (LitI i)) ie2@(BinopExpr GreaterThanEqual (Var y) (LitI j))
            | x == y    = gt ie1 (BinopExpr LessThan (Var y) (LitI (j-1)))
            | otherwise = dflt ie1 ie2
        gt ie1@(BinopExpr GreaterThanEqual (Var x) (LitI j)) ie2@(BinopExpr GreaterThan (Var y) (LitI i))
            | x == y    = gt ie1 (BinopExpr LessThan (Var y) (LitI (j-1)))
            | otherwise = dflt ie1 ie2
        gt ie1@(BinopExpr GreaterThan (Var x) (LitI i)) ie2@(BinopExpr Equal (Var y) (LitI j))
            | x == y    = if j > i then (True, opEqual (Var x) (LitI j)) else (True, LitB False)
            | otherwise = dflt ie1 ie2
        gt ie1@(BinopExpr Equal (Var x) (LitI j)) ie2@(BinopExpr GreaterThan (Var y) (LitI i))
            | x == y    = if j > i then (True, opEqual (Var x) (LitI j)) else (True, LitB False)
            | otherwise = dflt ie1 ie2
        gt ie1 ie2      = dflt ie1 ie2

        lte :: Expr -> Expr -> (Bool, Expr)
        lte ie1@(BinopExpr LessThanEqual (Var x) (LitI i)) ie2@(BinopExpr LessThanEqual (Var y) (LitI j))
            | x == y    = (True, opLessThanEqual (Var x) (LitI (min (i+1) (j+1))))
            | otherwise = dflt ie1 ie2
        lte ie1@(BinopExpr LessThanEqual (Var x) (LitI i)) ie2@(BinopExpr GreaterThanEqual (Var y) (LitI j))
            | x == y    = lt (opLessThan (Var x) (LitI (i+1))) ie2
            | otherwise = dflt ie1 ie2
        lte ie1@(BinopExpr GreaterThanEqual (Var x) (LitI j)) ie2@(BinopExpr LessThanEqual (Var y) (LitI i))
            | x == y    = lt (opLessThan (Var x) (LitI (i+1))) ie2
            | otherwise = dflt ie1 ie2
        lte ie1@(BinopExpr LessThanEqual (Var x) (LitI i)) ie2@(BinopExpr Equal (Var y) (LitI j))
            | x == y    = if j <= i then (True, opEqual (Var x) (LitI j)) else (True, LitB False)
            | otherwise = dflt ie1 ie2
        lte ie1@(BinopExpr Equal (Var x) (LitI j)) ie2@(BinopExpr LessThanEqual (Var y) (LitI i))
            | x == y    = if j <= i then (True, opEqual (Var x) (LitI j)) else (True, LitB False)
            | otherwise = dflt ie1 ie2
        lte ie1 ie2     = dflt ie1 ie2

        gte :: Expr -> Expr -> (Bool, Expr)
        gte ie1@(BinopExpr GreaterThanEqual (Var x) (LitI i)) ie2@(BinopExpr GreaterThanEqual (Var y) (LitI j))
            = gt (opGreaterThan (Var x) (LitI (i-1))) (opGreaterThan (Var y) (LitI (j-1)))
        gte ie1@(BinopExpr GreaterThanEqual (Var x) (LitI i)) ie2@(BinopExpr Equal (Var y) (LitI j))
            = gt (opGreaterThan (Var x) (LitI (i-1))) ie2
        gte ie1@(BinopExpr Equal (Var x) (LitI j)) ie2@(BinopExpr GreaterThanEqual (Var y) (LitI i))
            = gt ie1 (opGreaterThan (Var y) (LitI (i-1)))
        gte ie1 ie2
            = dflt ie1 ie2

        eql :: Expr -> Expr -> (Bool, Expr)
        eql ie1@(BinopExpr Equal (Var x) (LitI i)) ie2@(BinopExpr Equal (Var y) (LitI j))
            | x == y    = if i == j then (True, opEqual (Var x) (LitI i)) else (True, LitB False)
            | otherwise = dflt ie1 ie2
        eql ie1 ie2     = dflt ie1 ie2

or :: Expr -> Expr -> Expr
or (LitB True) _   = LitB True
or _ (LitB True)   = LitB True
or (LitB False) e  = e
or e (LitB False)  = e
or e1 e@(BinopExpr And e2 _) = if e1 == e2 then e1 else opOr e1 e
or e1 e2 
    | e1 == e2         = e1
    | fst (dist e1 e2) = snd (dist e1 e2)
    | fst (lt e1 e2)   = snd (lt e1 e2)
    | fst (gt e1 e2)   = snd (gt e1 e2)
    | fst (lte e1 e2)  = snd (lte e1 e2)
    | fst (gte e1 e2)  = snd (gte e1 e2)
    | otherwise        = opOr e1 e2
    where
        dist :: Expr -> Expr -> (Bool, Expr)
        dist (BinopExpr And x1 y1) (BinopExpr And x2 y2)
            | x1 == x2 = (True, 
                         opAnd x1 (opOr y1 y2))
            | y1 == y2 = (True,
                         opAnd y1 (opOr x1 x2))
        dist ie1 ie2   = (False, opOr ie1 ie2)

        dflt x y = (False, opOr x y)

        lt :: Expr -> Expr -> (Bool, Expr)
        lt ie1@(BinopExpr LessThan (Var x) (LitI i)) ie2@(BinopExpr LessThan (Var y) (LitI j))
            | x == y    = (True, opLessThan (Var x) (LitI (max i j)))
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr LessThan (Var x) (LitI i)) ie2@(BinopExpr LessThanEqual (Var y) (LitI j))
            | x == y    = lt ie1 (opLessThan (Var x) (LitI (j+1)))
            | otherwise = dflt ie1 ie2
        lt ie1@(BinopExpr LessThanEqual (Var x) (LitI i)) ie2@(BinopExpr LessThan (Var y) (LitI j))
            | x == y    = lt (opLessThan (Var x) (LitI (i+1))) ie2
            | otherwise = dflt ie1 ie2
        lt ie1 ie2      = dflt ie1 ie2

        gt :: Expr -> Expr -> (Bool, Expr)
        gt ie1@(BinopExpr GreaterThan (Var x) (LitI i)) ie2@(BinopExpr GreaterThan (Var y) (LitI j))
            | x == y    = (True, opGreaterThan (Var x) (LitI (min i j)))
            | otherwise = dflt ie1 ie2
        gt ie1@(BinopExpr GreaterThan (Var x) (LitI i)) ie2@(BinopExpr GreaterThanEqual (Var y) (LitI j))
                        = gt ie1 (opGreaterThan (Var y) (LitI (j-1)))
        gt ie1@(BinopExpr GreaterThanEqual (Var x) (LitI j)) ie2@(BinopExpr GreaterThan (Var y) (LitI i))
                        = gt ie2 ie1
        gt ie1 ie2      = dflt ie1 ie2

        lte :: Expr -> Expr -> (Bool, Expr)
        lte ie1@(BinopExpr LessThanEqual (Var x) (LitI i)) ie2@(BinopExpr LessThanEqual (Var y) (LitI j))
            | x == y    = (True, opLessThanEqual (Var x) (LitI (max (i+1) (j+1))))
            | otherwise = dflt ie1 ie2
        lte ie1 ie2     = dflt ie1 ie2

        gte :: Expr -> Expr -> (Bool, Expr)
        gte ie1@(BinopExpr GreaterThanEqual (Var x) (LitI i)) ie2@(BinopExpr GreaterThanEqual (Var y) (LitI j))
            = gt (opGreaterThan (Var x) (LitI (i-1))) (opGreaterThan (Var y) (LitI (j-1)))
        gte ie1 ie2
            = dflt ie1 ie2

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

minus :: Expr -> Expr -> Expr
minus (LitI x) (LitI y)                     = LitI (x-y)
minus (LitI x) (BinopExpr Minus e (LitI y)) = opMinus (LitI (x+y)) e
minus (BinopExpr Minus e (LitI y)) (LitI x) = opMinus e (LitI (y-x))
minus (LitI x) (BinopExpr Minus (LitI y) e) = opPlus (LitI (x-y)) e
minus (BinopExpr Minus (LitI y) e) (LitI x) = opMinus (LitI (y-x)) e
minus (LitI x) (BinopExpr Plus e (LitI y))  = opMinus (LitI (x-y)) e
minus (BinopExpr Plus e (LitI y)) (LitI x)  = opPlus e (LitI (y-x))
minus (LitI x) (BinopExpr Plus (LitI y) e)  = minus (LitI x) (opPlus e (LitI y))
minus (BinopExpr Plus (LitI y) e) (LitI x)  = minus (opPlus e (LitI y)) (LitI x)
minus e1       e2                           = opMinus e1 e2

plus :: Expr -> Expr -> Expr
plus (LitI x) (LitI y)                      = LitI (x+y)
plus (LitI x) (BinopExpr Minus e (LitI y))  = opMinus e (LitI (y+x))
plus (BinopExpr Minus e (LitI y)) (LitI x)  = opMinus e (LitI (y+x))
plus (LitI x) (BinopExpr Minus (LitI y) e)  = opMinus (LitI (x+y)) e
plus (BinopExpr Minus (LitI y) e) (LitI x)  = opMinus (LitI (x+y)) e
plus (LitI x) (BinopExpr Plus e (LitI y))   = opPlus e (LitI (x+y))
plus (BinopExpr Plus e (LitI y)) (LitI x)   = opPlus e (LitI (x+y))
plus (LitI x) (BinopExpr Plus (LitI y) e)   = opPlus e (LitI (x+y))
plus (BinopExpr Plus (LitI y) e) (LitI x)   = opPlus e (LitI (x+y))
plus e1         e2                          = opPlus e1 e2

multiply :: Expr -> Expr -> Expr
multiply (LitI x) (LitI y)                        = LitI (x*y)
multiply (LitI x) (BinopExpr Minus e (LitI y))    = opMinus (opMultiply (LitI x) e) (LitI (y*x))
multiply (BinopExpr Minus e (LitI y)) (LitI x)    = opMinus (opMultiply (LitI x) e) (LitI (y*x))
multiply (LitI x) (BinopExpr Minus (LitI y) e)    = opMinus (LitI (x*y)) (opMultiply (LitI x) e)
multiply (BinopExpr Minus (LitI y) e) (LitI x)    = opMinus (LitI (x*y)) (opMultiply (LitI x) e)
multiply (LitI x) (BinopExpr Plus e (LitI y))     = opPlus (opMultiply (LitI x) e) (LitI (x*y))
multiply (BinopExpr Plus e (LitI y)) (LitI x)     = opPlus (opMultiply (LitI x) e) (LitI (x*y))
multiply (LitI x) (BinopExpr Plus (LitI y) e)     = opPlus (opMultiply (LitI x) e) (LitI (x*y))
multiply (BinopExpr Plus (LitI y) e) (LitI x)     = opPlus (opMultiply (LitI x) e) (LitI (x*y))
multiply (LitI x) (BinopExpr Multiply e (LitI y)) = opMultiply e (LitI (x*y))
multiply (BinopExpr Multiply e (LitI y)) (LitI x) = opMultiply e (LitI (x*y))
multiply (LitI x) (BinopExpr Multiply (LitI y) e) = opMultiply e (LitI (x*y))
multiply (BinopExpr Multiply (LitI y) e) (LitI x) = opMultiply e (LitI (x*y))
multiply (LitI x) (BinopExpr Divide e (LitI y))   = opDivide (opMultiply (LitI x) e) (LitI y)
multiply (BinopExpr Divide e (LitI y)) (LitI x)   = opDivide (opMultiply (LitI x) e) (LitI y)
multiply (LitI x) (BinopExpr Divide (LitI y) e)   = opDivide (LitI (x*y)) e
multiply (BinopExpr Divide (LitI y) e) (LitI x)   = opDivide (LitI (x*y)) e
multiply e1         e2                            = opMultiply e1 e2

divide :: Expr -> Expr -> Expr
divide (LitI x) (LitI y)                        = LitI (x `div` y)
divide (BinopExpr Minus e (LitI y)) (LitI x)    = opMinus (opDivide e (LitI x)) (LitI (y `div` x))
divide (BinopExpr Minus (LitI y) e) (LitI x)    = opMinus (LitI (y `div` x)) (opDivide e (LitI x))
divide (BinopExpr Plus e (LitI y)) (LitI x)     = opPlus (opDivide e (LitI x)) (LitI (y `div` x))
divide (BinopExpr Plus (LitI y) e) (LitI x)     = opPlus (opDivide e (LitI x)) (LitI (y `div` x))
divide (LitI x) (BinopExpr Multiply e (LitI y)) = opMultiply (opDivide (LitI 1) e) (LitI (x `div` y))
divide (BinopExpr Multiply e (LitI y)) (LitI x) = opMultiply e (LitI (y `div` x))
divide (LitI x) (BinopExpr Multiply (LitI y) e) = opMultiply (opDivide (LitI 1) e) (LitI (x `div` y))
divide (BinopExpr Multiply (LitI y) e) (LitI x) = opMultiply e (LitI (y `div` x))
divide (LitI x) (BinopExpr Divide e (LitI y))   = opDivide (LitI (x*y)) e
divide (BinopExpr Divide e (LitI y)) (LitI x)   = opDivide e (LitI (x*y))
divide (LitI x) (BinopExpr Divide (LitI y) e)   = opMultiply (LitI (x `div` y)) e
divide (BinopExpr Divide (LitI y) e) (LitI x)   = opMultiply (LitI (y `div` x)) (opDivide (LitI 1) e)
divide e1         e2                            = opMultiply e1 e2