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