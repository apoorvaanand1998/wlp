module Paths (PathTree(..), Statement(..), programTree, singleton) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.List (intercalate)
import GCLParser.GCLDatatype
import Data.Function (on)
import qualified GCLParser.Parser as GCLP
import Options (Opts (maxDepth))



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

data PathTree = Terminate
              | Stmt !Statement !PathTree
              | Branch !PathTree !PathTree

instance Show PathTree where
  show Terminate = "\n└─🛑"
  show (Stmt s t) = "\n" ++ show s ++ show t
  show (Branch t1 t2) = "\n├── " ++ intercalate "\n│   " (drop 1 $ lines $ show t1)
                     ++ "\n└── " ++ intercalate "\n    " (drop 1 $ lines $ show t2)

  
programTree :: Opts -> Program -> (PathTree, [PathTree])
programTree opts Program { input, output, stmt } = runWriter (evalStateT (runReaderT (tree (Block (input ++ output) stmt)) opts) 0)

(|>) :: PathTree -> PathTree -> PathTree
Terminate |> t2 = t2
Stmt s t1 |> t2 = Stmt s (t1 |> t2)
Branch t1 t2 |> t3 = Branch (t1 |> t3) (t2 |> t3)

singleton :: Statement -> PathTree
singleton s = Stmt s Terminate

tree :: Stmt -> ReaderT Opts (StateT Int (Writer [PathTree])) PathTree
tree Skip = pure Terminate
tree (Assert cond) = pure $ singleton (SAssert cond)
tree (Assume cond) = pure $ singleton (SAssume cond)
tree (Assign var expr) = pure $ singleton (SAssign var expr)
tree (AAssign arr idx expr) = pure $ singleton (SAAssign arr idx expr)
tree (DrefAssign _var _expr) = error "out of scope?"

-- while loop with invariant
tree (Seq (Assert inv) (While guard body)) = do
  tbody <- pruned (tree body)
  tell [singleton (SAssume inv) |> singleton (SAssume guard) |> tbody |> singleton (SAssert inv)]
  pure $ singleton (SAssert inv)
tree (Seq (Assert inv) (Seq (While guard body) rest)) = do
  t1 <- pruned (tree (Seq (Assert inv) (While guard body)))
  t2 <- pruned (tree rest)
  pure (t1 |> t2)

tree (Seq expr1 expr2) = do
    t1 <- pruned (tree expr1)
    t2 <- pruned (tree expr2)
    pure (t1 |> t2)
tree (IfThenElse guard true false) = do
  ttrue  <- (singleton (SAssume guard) |>)         <$> pruned (tree true)
  tfalse <- (singleton (SAssume (OpNeg guard)) |>) <$> pruned (tree false)
  pure (Branch ttrue tfalse)
tree (While guard body) = do
  tbody <- pruned (tree body)
  tloop <- pruned (tree (While guard body))
  let ttrue  = singleton (SAssume guard) |> tbody |> tloop
  let tfalse = singleton (SAssume (OpNeg guard))
  pure (Branch ttrue tfalse)
tree (Block [] stmt) = pruned (tree stmt)
tree (Block (VarDeclaration s _:xs) stmt) = do
  n <- fresh
  t <- pruned (tree (Block xs stmt))
  pure (rename s n t)
tree (TryCatch _catch _try _expr) = error "out of scope?"



fresh :: MonadState Int m => m String
fresh = do
    n <- gets varName
    modify succ
    pure n

varName :: Int -> String
varName i = 'x' : map (("₀₁₂₃₄₅₆₇₈₉" !!) . read . pure) (show i)

pruned :: MonadReader Opts m => m PathTree -> m PathTree
pruned mtree = do
  k <- asks maxDepth
  if k > 0
    then local (\opts -> opts { maxDepth = pred k }) mtree
    else pure Terminate



class Renamable a where rename :: String -> String -> a -> a

instance Renamable PathTree where
  rename _ _ Terminate = Terminate
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
