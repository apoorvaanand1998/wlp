module Basic where

import Control.Monad.Reader
import Control.Monad.State
import GCLParser.GCLDatatype

type Todo = ()

-- Probably need to annotate this eventually to have useful output
data Statement
    = SAssert Expr
    | SAssume Expr
    | SAssign String Expr
    | SAAssign String Expr Expr

type Path = [Statement]


-- |Not sure if this works for our purpose...
programPaths :: Int -> Program -> [Path]
programPaths k Program { input, stmt } = undefined -- paths k 0 (Block input stmt)

data Thing = Thing { k :: Int, i :: Int }

shorter :: Reader Thing a -> Reader Thing a
shorter = local (\(Thing k i) -> Thing (k-1) i)


type PathExtractor = StateT () (Reader Thing)

{-



-}

-- |Naïvely finds ALL paths of a program
--  WARNING: this function is explosive at the moment!
paths :: Stmt -> Reader Thing [Path]
paths _ | False {- k == 0 -} = return []
paths Skip = return [mempty]
paths (Assert cond) = return [pure (SAssert cond)]
paths (Assume cond) = return [pure (SAssume cond)]
paths (Assign var expr) = return [pure (SAssign var expr)]
paths (AAssign arr idx expr) = return [pure (SAAssign arr idx expr)]
paths (DrefAssign _var _expr) = error "out of scope?"
paths (Seq expr1 expr2) = do
  xs <- paths expr1
  ys <- paths expr2
  return $ liftA2 (++) xs ys
paths (IfThenElse guard true false) = shorter $ do
  xs <- fmap (SAssume guard :) <$> paths true
  ys <- fmap (SAssume (OpNeg guard) :) <$> paths false
  return $ xs ++ ys
paths (While guard body) = shorter $ do
  xs <- fmap (SAssume guard :) <$> paths body
  return $ [SAssume (OpNeg guard)] : xs
paths (Block [] stmt) = paths stmt
paths (Block (x:xs) stmt) = do
  -- ys <- paths (Block xs stmt)
  -- return $ fmap (x :) ys
  return [[]]
paths (TryCatch _catch _try _expr) = error "out of scope?"

rename :: String -> String -> Stmt -> Stmt
rename = undefined
