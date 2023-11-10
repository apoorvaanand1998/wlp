module Utils (countAtoms) where

import qualified Paths as P
import qualified GCLParser.GCLDatatype as GCLD
import GCLParser.GCLDatatype (Stmt)

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