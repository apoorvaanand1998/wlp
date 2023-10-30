module Utils (treeStmt) where

import qualified Paths as P
import qualified GCLParser.GCLDatatype as GCLD
import GCLParser.GCLDatatype (Stmt)

sToS :: P.Statement -> GCLD.Stmt
sToS (P.SAssert e)        = GCLD.Assert e
sToS (P.SAssume e)        = GCLD.Assume e
sToS (P.SAssign s e)      = GCLD.Assign s e
sToS (P.SAAssign s e1 e2) = GCLD.AAssign s e1 e2

treeStmt :: P.Tree P.Statement -> P.Tree Stmt
treeStmt st = sToS <$> st

-- not converting first to dnf/cnf because that's a np-hard problem apparently
countAtoms :: GCLD.Expr -> Int
countAtoms (GCLD.Parens e)   = countAtoms e
countAtoms (GCLD.OpNeg _)    = 1
countAtoms (GCLD.Forall _ e) = countAtoms e
countAtoms (GCLD.Exists _ e) = countAtoms e
countAtoms (GCLD.BinopExpr b e1 e2) = 
                               case b of
                                    GCLD.And -> 1 + countAtoms e1 + countAtoms e2
                                    GCLD.Or  -> 1 + countAtoms e1 + countAtoms e2
                                    _        -> countAtoms e1 + countAtoms e2
countAtoms _                 = 0