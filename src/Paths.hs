module Paths (PathTree(..), Statement(..), programTree, limitDepth, singleton) where

import Control.Monad.State
import Data.List (intercalate)
import GCLParser.GCLDatatype
import Data.Function (on)
import qualified GCLParser.Parser as GCLP

-- Probably need to annotate this eventually to have useful output
data Statement
    = SAssert !Expr
    | SAssume !Expr
    | SAssign !String !Expr
    | SAAssign !String !Expr !Expr

instance Show Statement where
    show (SAssert expr) = unwords ["ASSERT", show expr]
    show (SAssume expr) = unwords ["ASSUME", show expr]
    show (SAssign var expr) = show (Assign var expr)
    show (SAAssign arr idx expr) = show (AAssign arr idx expr)

data PathTree = Terminate | Crash | Prune
              | Stmt !Statement !PathTree
              | Branch !PathTree !PathTree

instance Show PathTree where
  show Terminate = "\n└─🛑"
  show Crash = "\n└─💥"
  show Prune = "\n└─✂️"
  show (Stmt s t) = "\n" ++ show s ++ show t
  show (Branch t1 t2) = "\n├── " ++ intercalate "\n│   " (drop 1 $ lines $ show t1)
                     ++ "\n└── " ++ intercalate "\n    " (drop 1 $ lines $ show t2)

limitDepth :: Int -> PathTree -> PathTree
limitDepth 0 _ = Stmt (SAssert $ LitB True) Prune
limitDepth _ Terminate = Terminate
limitDepth _ Crash = Crash
limitDepth _ Prune = Prune
limitDepth k (Stmt s t) = Stmt s (limitDepth (k-1) t)
limitDepth k (Branch t1 t2) = Branch (limitDepth (k-1) t1) (limitDepth (k-1) t2)

programTree :: Program -> PathTree
programTree Program { input, output, stmt } = evalState (tree (Block (input ++ output) stmt)) 0

(|>) :: PathTree -> PathTree -> PathTree
Terminate |> t2 = t2
Crash |> _ = Crash
Prune |> _ = Prune
Stmt s t1 |> t2 = Stmt s (t1 |> t2)
Branch t1 t2 |> t3 = Branch (t1 |> t3) (t2 |> t3)

singleton :: Statement -> PathTree
singleton s = Stmt s Terminate

tree :: Stmt -> State Int PathTree
tree Skip = pure Terminate
tree (Assert cond) = pure $ singleton (SAssert cond)
tree (Assume cond) = pure $ singleton (SAssume cond)
tree (Assign var expr) = pure $ singleton (SAssign var expr)
tree (AAssign arr idx expr) = pure $ singleton (SAAssign arr idx expr)
tree (DrefAssign _var _expr) = error "out of scope?"
tree (Seq expr1 expr2) = do
    t1 <- tree expr1
    t2 <- tree expr2
    pure (t1 |> t2)
tree (IfThenElse guard true false) = do
  ttrue  <- (singleton (SAssume guard) |>)         <$> tree true
  tfalse <- (singleton (SAssume (OpNeg guard)) |>) <$> tree false
  pure (Branch ttrue tfalse)
tree (While guard body) = do
  tbody <- tree body
  tloop <- tree (While guard body)
  let ttrue  = singleton (SAssume guard) |> tbody |> tloop
  let tfalse = singleton (SAssume (OpNeg guard))
  pure (Branch ttrue tfalse)
tree (Block [] stmt) = tree stmt
tree (Block (VarDeclaration s _:xs) stmt) = do
  n <- fresh
  t <- tree (Block xs stmt)
  pure (rename s n t)
tree (TryCatch _catch _try _expr) = error "out of scope?"

fresh :: State Int String
fresh = do
    n <- gets varName
    modify succ
    pure n



varName :: Int -> String
varName i = 'x' : map (("₀₁₂₃₄₅₆₈₉" !!) . read . pure) (show i)



class Renamable a where rename :: String -> String -> a -> a

instance Renamable PathTree where
  -- problem: when the original program contains any variable with a number as a name...
  -- does that happen?
  rename _ _ Terminate = Terminate
  rename _ _ Crash     = Crash
  rename _ _ Prune     = Prune
  rename new old (Stmt s t) = Stmt (rename new old s) (rename new old t)
  rename new old (Branch t1 t2) = (Branch `on` rename new old) t1 t2

instance Renamable Statement where
  rename new old (SAssert expr) = SAssert $ rename new old expr
  rename new old (SAssume expr) = SAssume $ rename new old expr
  rename new old (SAssign var expr) | var == old = SAssign new $ rename new old expr
                                    | otherwise  = SAssign var $ rename new old expr
  rename new old (SAAssign arr idx expr) | arr == old = SAAssign new idx $ rename new old expr
                                         | otherwise  = SAAssign arr idx $ rename new old expr

instance Renamable Expr where
  rename = GCLP.rename

-- myRename :: String -> String -> Expr -> Expr
-- myRename rep by (OpNeg expr) = OpNeg (myRename rep by expr)
-- myRename rep by (Var var) | rep == var = Var by
--                             | otherwise  = Var var
-- myRename _ _ (LitI i) = LitI i
-- myRename _ _ (LitB b) = LitB b
-- myRename _ _ LitNull  = LitNull
-- myRename rep by (Parens expr) = Parens (myRename rep by expr)
-- myRename rep by (ArrayElem expr1 expr2) = ArrayElem (myRename rep by expr1) (myRename rep by expr2)
-- myRename rep by (BinopExpr op expr1 expr2) = BinopExpr op (myRename rep by expr1) (myRename rep by expr2)
-- myRename rep by (Forall var expr) | rep == var = Forall by (myRename rep by expr)
--                                     | otherwise  = Forall var (myRename rep by expr)
-- myRename rep by (Exists var expr) | rep == var = Exists by (myRename rep by expr)
--                                     | otherwise  = Exists var (myRename rep by expr)
-- myRename rep by (SizeOf expr) = SizeOf (myRename rep by expr)
-- myRename rep by (RepBy expr1 expr2 expr3) = RepBy (myRename rep by expr1) (myRename rep by expr2) (myRename rep by expr3)
-- myRename rep by (Cond expr1 expr2 expr3) = Cond (myRename rep by expr1) (myRename rep by expr2) (myRename rep by expr3)
-- myRename rep by (NewStore expr) = NewStore (myRename rep by expr)
-- myRename rep by (Dereference var) | rep == var = Dereference by
--                                     | otherwise  = Dereference var
