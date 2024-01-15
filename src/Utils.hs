module Utils (countAtoms) where

import qualified GCLParser.GCLDatatype as GCLD



countAtoms :: GCLD.Expr -> Int
countAtoms (GCLD.Parens e)   = countAtoms e
countAtoms (GCLD.OpNeg e)    = countAtoms e
countAtoms (GCLD.Forall _ e) = countAtoms e
countAtoms (GCLD.Exists _ e) = countAtoms e

countAtoms (GCLD.BinopExpr binop e1 e2)
  | boolean binop = countAtoms e1 + countAtoms e2
  | otherwise     = countAtoms e1 + countAtoms e2 - 1

countAtoms (GCLD.Var _)           = 1
countAtoms (GCLD.LitB _)          = 1
countAtoms (GCLD.LitI _)          = 1
countAtoms GCLD.LitNull           = 1
countAtoms (GCLD.ArrayElem e1 e2) = countAtoms e1 + countAtoms e2 - 1
countAtoms (GCLD.SizeOf e)        = countAtoms e

countAtoms (GCLD.RepBy e1 e2 e3)  = countAtoms e1 + countAtoms e2 + countAtoms e3
countAtoms (GCLD.Cond e1 e2 e3)   = countAtoms e1 + countAtoms e2 + countAtoms e3
countAtoms (GCLD.NewStore e)      = countAtoms e
countAtoms (GCLD.Dereference _)   = 1



boolean :: GCLD.BinOp -> Bool
boolean GCLD.And = True
boolean GCLD.Or  = True
boolean GCLD.Implication = True
boolean _ = False
