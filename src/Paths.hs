module Paths (Tree(..), Node(..), Statement(..), programTree, limitDepth) where

import GCLParser.GCLDatatype
import GCLParser.Parser
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

-- |Cuts of branches that are too long, and replaces them with "ASSERT FALSE"
limitDepth :: Int -> Tree Statement -> Tree Statement
limitDepth 0 _ = Tree [Node (SAssert (LitB False)) mempty]
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
  pure (renameTree s n t)
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
renameTree :: String -> String -> Tree Statement -> Tree Statement
renameTree new old = fmap (renameStmt new old)

renameStmt :: String -> String -> Statement -> Statement
renameStmt new old (SAssert expr) = SAssert $ rename new old expr
renameStmt new old (SAssume expr) = SAssume $ rename new old expr
renameStmt new old (SAssign var expr) | var == old = SAssign new $ rename new old expr
                                     | otherwise  = SAssign var $ rename new old expr
renameStmt new old (SAAssign arr idx expr) | arr == old = SAAssign new idx $ rename new old expr
                                          | otherwise  = SAAssign arr idx $ rename new old expr

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
