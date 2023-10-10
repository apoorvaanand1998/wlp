module Paths (programTree) where

import GCLParser.GCLDatatype

-- Probably need to annotate this eventually to have useful output
data Statement
    = SAssert Expr
    | SAssume Expr
    | SAssign String Expr
    | SAAssign String Expr Expr

newtype Tree a = Tree [Node a] deriving (Semigroup, Monoid, Functor, Show)
data Node a = Node a (Tree a) deriving (Functor, Show)

-- an alternative Semigroup instance
-- instance Semigroup (Tree a) where
--   Tree xss <> ys = Tree $ map (\(Node x xs) -> Node x (xs <> ys)) xss

-- instance Applicative Tree where
--   pure x = Tree [pure x]
--   Tree fs <*> Tree xs = Tree $ liftA2 (<*>) fs xs

-- instance Applicative Node where
--   pure x = Node x mempty
--   Node f fs <*> b@(Node x xs) = Node (f x) ((f <$> xs) <> (fs <*> Tree [b]))


programTree :: Program -> Tree Statement
programTree Program { input, stmt } = tree (Block input stmt) mempty

-- Insert a new root above the current root of the tree
(|>) :: a -> Tree a -> Tree a
x |> t = Tree [Node x t]

tree :: Stmt -> Tree Statement -> Tree Statement
tree Skip = id
tree (Assert cond) = (SAssert cond |>)
tree (Assume cond) = (SAssume cond |>)
tree (Assign var expr) = (SAssign var expr |>)
tree (AAssign arr idx expr) = (SAAssign arr idx expr |>)
tree (DrefAssign _var _expr) = error "out of scope?"
tree (Seq expr1 expr2) = tree expr1 . tree expr2
tree (IfThenElse guard true false)
  = (SAssume guard         |>) . tree true
 <> (SAssume (OpNeg guard) |>) . tree false
tree (While guard body)
  = (SAssume (OpNeg guard) |>)
 <> (SAssume guard |>) . tree body . tree (While guard body)
tree (Block xs stmt) = foldMap f xs . tree stmt
  where f :: VarDeclaration -> Tree Statement -> Tree Statement
        f = error "todo: what to do with var declaration?"
tree (TryCatch _catch _try _expr) = error "out of scope?"
