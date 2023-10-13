module Paths (Tree(..), Statement(..), programTree, limitDepth) where

import GCLParser.GCLDatatype
import Data.List (intercalate)
import Control.Monad.State

-- Probably need to annotate this eventually to have useful output
data Statement
    = SAssert Expr
    | SAssume Expr
    | SAssign String Expr
    | SAAssign String Expr Expr

instance Show Statement where
  show (SAssert expr) = unwords ["ASSERT", show expr]
  show (SAssume expr) = unwords ["ASSUME", show expr]
  show (SAssign var expr) = show (Assign var expr)
  show (SAAssign arr idx expr) = show (AAssign arr idx expr)

newtype Tree a = Tree [Node a] deriving (Semigroup, Monoid, Functor)
data Node a = Node a (Tree a) deriving (Functor)

instance Show a => Show (Tree a) where
  show (Tree []) = "\n└─■"
  show (Tree [n])    = (("\n└── " ++) . intercalate "\n    " . lines) (show n)
  show (Tree (n:ns)) = (("\n├── " ++) . intercalate "\n│   " . lines) (show n) ++ show (Tree ns)

instance Show a => Show (Node a) where
  show (Node x (Tree [n])) = show x ++ "\n" ++ show n
  show (Node x t) = show x ++ show t

limitDepth :: Int -> Tree a -> Tree a
limitDepth 0 _ = Tree []
limitDepth k (Tree xs) = Tree (map f xs)
  where f (Node n ns) = Node n (limitDepth (k-1) ns)

programTree :: Program -> (Tree Statement, Int)
programTree Program { input, output, stmt } = runState (tree (Block (input ++ output) stmt)) 0

(|>) :: Tree a -> Tree a -> Tree a
Tree [] |> t2 = t2
Tree ns |> t2 = Tree $ fmap (\(Node x t) -> Node x (t |> t2)) ns

singleton :: a -> Tree a
singleton x = Tree [Node x mempty]

tree :: Stmt -> State Int (Tree Statement)
tree Skip = pure mempty
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
  pure (ttrue <> tfalse)
tree (While guard body) = do
  tbody <- tree body
  tloop <- tree (While guard body)
  let ttrue  = singleton (SAssume guard) |> tbody |> tloop
  let tfalse = singleton (SAssume (OpNeg guard))
  pure (ttrue <> tfalse)
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
varName i = 'x' : map ((digs !!) . read . pure) (show i)
  where digs = "₀₁₂₃₄₅₆₈₉"

-- problem: when the original program contains any variable with a number as a name...
-- does that happen?
rename :: String -> String -> Tree Statement -> Tree Statement
rename rep by = fmap (renameStmt rep by)

renameStmt :: String -> String -> Statement -> Statement
renameStmt rep by (SAssert expr) = SAssert $ renameExpr rep by expr
renameStmt rep by (SAssume expr) = SAssume $ renameExpr rep by expr
renameStmt rep by (SAssign var expr) | rep == var = SAssign by $ renameExpr rep by expr
                                     | otherwise  = SAssign var $ renameExpr rep by expr
renameStmt rep by (SAAssign arr idx expr) | rep == arr = SAAssign by idx $ renameExpr rep by expr
                                          | otherwise  = SAAssign arr idx $ renameExpr rep by expr

renameExpr :: String -> String -> Expr -> Expr
renameExpr rep by (OpNeg expr) = OpNeg (renameExpr rep by expr)
renameExpr rep by (Var var) | rep == var = Var by
                            | otherwise  = Var var
renameExpr _ _ (LitI i) = LitI i
renameExpr _ _ (LitB b) = LitB b
renameExpr _ _ LitNull  = LitNull
renameExpr rep by (Parens expr) = Parens (renameExpr rep by expr)
renameExpr rep by (ArrayElem expr1 expr2) = ArrayElem (renameExpr rep by expr1) (renameExpr rep by expr2)
renameExpr rep by (BinopExpr op expr1 expr2) = BinopExpr op (renameExpr rep by expr1) (renameExpr rep by expr2)
renameExpr rep by (Forall var expr) | rep == var = Forall by (renameExpr rep by expr)
                                    | otherwise  = Forall var (renameExpr rep by expr)
renameExpr rep by (Exists var expr) | rep == var = Exists by (renameExpr rep by expr)
                                    | otherwise  = Exists var (renameExpr rep by expr)
renameExpr rep by (SizeOf expr) = SizeOf (renameExpr rep by expr)
renameExpr rep by (RepBy expr1 expr2 expr3) = RepBy (renameExpr rep by expr1) (renameExpr rep by expr2) (renameExpr rep by expr3)
renameExpr rep by (Cond expr1 expr2 expr3) = Cond (renameExpr rep by expr1) (renameExpr rep by expr2) (renameExpr rep by expr3)
renameExpr rep by (NewStore expr) = NewStore (renameExpr rep by expr)
renameExpr rep by (Dereference var) | rep == var = Dereference by
                                    | otherwise  = Dereference var
