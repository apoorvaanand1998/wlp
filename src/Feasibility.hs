module Feasibility where

import Paths
import GCLParser.GCLDatatype
import Verification

-- ^ Remove paths for which given precondition `p` don't hold
pruneInfeasible :: Expr -> Tree Statement -> Tree Statement
pruneInfeasible p (Tree [])
    | satisfiable p  = Tree []
    | otherwise      = pruned
pruneInfeasible p (Tree (Node (SAssume x) t:ns))
    | satisfiable p' = pruneInfeasible p' t <> pruneInfeasible p (Tree ns)
    | otherwise      = pruned               <> pruneInfeasible p (Tree ns)
  where p' = BinopExpr And p x
pruneInfeasible p (Tree (Node (SAssert x) t:ns)) = undefined
pruneInfeasible p (Tree (Node (SAssign x e) t:ns)) = undefined
pruneInfeasible p (Tree (Node (SAAssign x i e) t:ns)) = undefined

-- ^ I think this is how we want to prune inevasible paths? Just replace them with ASSERT FALSE?
pruned :: Tree Statement
pruned = singleton (SAssert (LitB False))

substitute :: Expr -> String -> Expr -> Expr
substitute e x (OpNeg expr) = OpNeg (substitute e x expr)
substitute e x (Var var) | x == var = e
                         | otherwise = Var var
substitute _ _ (LitI i) = LitI i
substitute _ _ (LitB b) = LitB b
substitute _ _ LitNull  = LitNull
substitute e x (Parens expr) = Parens (substitute e x expr)
substitute e x (ArrayElem expr1 expr2) = ArrayElem (substitute e x expr1) (substitute e x expr2)
substitute e x (BinopExpr op expr1 expr2) = BinopExpr op (substitute e x expr1) (substitute e x expr2)
substitute e x (Forall var expr) = error "what?"
substitute e x (Exists var expr) = error "what?"
substitute e x (SizeOf expr) = SizeOf (substitute e x expr)
substitute e x (RepBy expr1 expr2 expr3) = RepBy (substitute e x expr1) (substitute e x expr2) (substitute e x expr3)
substitute e x (Cond expr1 expr2 expr3) = Cond (substitute e x expr1) (substitute e x expr2) (substitute e x expr3)
substitute e x (NewStore expr) = NewStore (substitute e x expr)
substitute e x (Dereference var) = error "what?"

satisfiable :: Expr -> IO Bool
satisfiable pred  = do
    res <- checkValid env pred
    print res
  where
    env = mkEnv $ getTypes pred
