module Paths (Tree(..), Statement(..), programTree, limitDepth) where

import GCLParser.GCLDatatype
import Data.List (intercalate)

-- Probably need to annotate this eventually to have useful output
data Statement
    = SAssert Expr
    | SAssume Expr
    | SAssign String Expr
    | SAAssign String Expr Expr

instance Show Statement where
  show (SAssert expr) = unwords ["ASSERT", show expr]
  show (SAssume expr) = unwords ["ASSUME", show expr]
  show (SAssign var expr) = unwords ["ASSIGN", var, show expr]
  show (SAAssign arr idx expr) = unwords ["AASSIGN", arr, show idx, show expr]

newtype Tree a = Tree [Node a] deriving (Semigroup, Monoid, Functor)
data Node a = Node a (Tree a) deriving (Functor)

instance Show a => Show (Tree a) where
  show (Tree []) = "\n└─■"
  show (Tree [n])    = (("\n└── " ++) . intercalate "\n    " . lines) (show n)
  show (Tree (n:ns)) = (("\n├── " ++) . intercalate "\n│   " . lines) (show n) ++ show (Tree ns)

instance Show a => Show (Node a) where
  show (Node x t) = show x ++ show t

limitDepth :: Int -> Tree a -> Tree a
limitDepth 0 _ = Tree []
limitDepth k (Tree xs) = Tree (map f xs)
  where f (Node n ns) = Node n (limitDepth (k-1) ns)

programTree :: Program -> Tree Statement
programTree Program { input, output, stmt } = tree 0 (Block (input ++ output) stmt) mempty

-- Insert a new root above the current root of the tree
(|>) :: a -> Tree a -> Tree a
x |> t = Tree [Node x t]

tree :: Int -> Stmt -> Tree Statement -> Tree Statement
tree _ Skip = id
tree _ (Assert cond) = (SAssert cond |>)
tree _ (Assume cond) = (SAssume cond |>)
tree _ (Assign var expr) = (SAssign var expr |>)
tree _ (AAssign arr idx expr) = (SAAssign arr idx expr |>)
tree _ (DrefAssign _var _expr) = error "out of scope?"
tree i (Seq expr1 expr2) = tree i expr1 . tree i expr2
tree i (IfThenElse guard true false)
  = (SAssume guard         |>) . tree i true
 <> (SAssume (OpNeg guard) |>) . tree i false
tree i (While guard body)
  = (SAssume (OpNeg guard) |>)
 <> (SAssume guard |>) . tree i body . tree i (While guard body)
tree i (Block [] stmt) = tree i stmt
tree i (Block (VarDeclaration s _:xs) stmt) = rename s (varName i) . tree (succ i) (Block xs stmt)
tree _ (TryCatch _catch _try _expr) = error "out of scope?"

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
renameExpr _ _ l@(LitI _)  = l
renameExpr _ _ l@(LitB _)  = l
renameExpr _ _ l@(LitNull) = l
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
