{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Verification where

import Options
import Paths

import Control.Monad (join)
import Control.Monad.Reader (ReaderT(..), asks, local)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GCLParser.GCLDatatype
import System.CPUTime (getCPUTime)
import Z3.Monad hiding (Opts, local)

-- ^ A environment mapping variables to Z3 ASTs, each variable is stored as an @Int@
type Env = Map String AST

-- We cant use lists since PrimitiveType does not implement Ord
mkArraySorts :: Z3 [(PrimitiveType, Sort)]
mkArraySorts = do
    intSort <- mkIntSort
    boolSort <- mkBoolSort
    sequenceA
        [ (PTInt,) <$> mkArraySort intSort intSort
        , (PTBool,) <$> mkArraySort intSort boolSort
        ]

-- ^ Takes a PrimitiveType and returns a Type
toType :: PrimitiveType -> Type
toType = PType

getTypes :: Expr -> Map String Type
getTypes = \case
    Var v -> M.singleton v RefType
    LitI _ -> M.empty
    LitB _ -> M.empty
    LitNull -> M.empty
    Parens e -> getTypes e
    ArrayElem a i -> M.union (getTypes a) (getTypes i)
    OpNeg e -> getTypes e
    BinopExpr _ lhs rhs -> M.union (getTypes lhs) (getTypes rhs)
    Forall i e -> M.insert i (PType PTInt) (getTypes e)
    Exists i e -> M.insert i (PType PTInt) (getTypes e)
    SizeOf e -> M.singleton ('#' : e') (PType PTInt)
      where
        e' = case e of
            Var v -> v
            _ -> error "SizeOf should only be called on a variable"
    RepBy e1 e2 e3 -> M.unions [getTypes e1, getTypes e2, getTypes e3]
    Cond g e1 e2 -> M.unions [getTypes g, getTypes e1, getTypes e2]
    NewStore _ -> undefined
    Dereference _ -> undefined

mkEnv :: Map String Type -> Z3 Env
mkEnv env = mkArraySorts >>= \arr -> do
    lengthVars <- mkLengthVars env
    vars <- mkVars arr env
    return $ M.union lengthVars vars
  where
    mkLengthVars :: Map String Type -> Z3 Env
    mkLengthVars =
        fmap M.fromList
        . traverse (\x -> ("#" <> x,) <$> mkFreshIntVar ("#" <> x))
        . M.keys
        -- Only handle arrays
        . M.filter (\case
            AType _ -> True
            _ -> False
        )
    -- Could be a lot easier if we used Map instead of of a list 🤷‍♂️
    mkVars :: [(PrimitiveType, Sort)] -> Map String Type -> Z3 Env
    mkVars xs = M.traverseWithKey (\i t -> case t of
        PType PTInt -> mkFreshIntVar i
        PType PTBool -> mkFreshBoolVar i
        RefType -> error "RefType should not be in the environment"
        AType t' -> mkFreshVar i $ fromJust (t' `lookup` xs))


-- ^ Takes an operator and two expressions and returns a Z3 AST
convertOp :: BinOp -> AST -> AST -> ReaderT Env Z3 AST
convertOp op lhs rhs = case op of
    And -> mkAnd [lhs, rhs]
    Or -> mkOr [lhs, rhs]
    Implication -> mkImplies lhs rhs
    LessThan -> mkLt lhs rhs
    LessThanEqual -> mkLe lhs rhs
    GreaterThan -> mkGt lhs rhs
    GreaterThanEqual -> mkGe lhs rhs
    Equal -> mkEq lhs rhs
    Minus -> mkSub [lhs, rhs]
    Plus -> mkAdd [lhs, rhs]
    Multiply -> mkMul [lhs, rhs]
    Divide -> mkDiv lhs rhs
    Alias -> undefined -- not sure we need this

-- ^ Takes an expression and returns a Z3 AST
-- There is probably a better way to do this, possible using catastrophism
convert :: Expr -> (ReaderT Env Z3) AST
convert expr = case expr of
    Var v -> asks (M.! v)
    LitI i -> mkInteger $ toInteger i
    LitB b -> mkBool b
    LitNull -> error "null should never happen at this point"
    Parens e -> e'
      where
        e' = convert e
    ArrayElem a i -> join $ mkSelect <$> a' <*> i'
      where
        a' = convert a
        i' = convert i
    OpNeg e -> mkUnaryMinus =<< e'
      where
        e' = convert e
    BinopExpr o lhs rhs -> join $ convertOp o <$> lhs' <*> rhs'
      where
        lhs' = convert lhs
        rhs' = convert rhs
    Forall i e -> do
        var <- mkFreshIntVar i
        app <- toApp var
        mkForallConst [] [app] =<< local (M.insert i var) e'
      where
        e' = convert e
    Exists i e -> do
        var <- mkFreshIntVar i
        app <- toApp var
        mkExistsConst [] [app] =<< local (M.insert i var) e'
      where
        e' = convert e
    SizeOf e -> case e of
        Var v -> asks (M.! ('#' : v))
        _ -> error "SizeOf should only be called on a variable"
    RepBy e1 e2 e3 -> join $ mkStore <$> e1' <*> e2' <*> e3'
      where
        e1' = convert e1
        e2' = convert e2
        e3' = convert e3
    Cond g e1 e2 -> convert e'
      where
        e1' = BinopExpr And g e1
        e2' = BinopExpr And (OpNeg g) e2
        e'  = BinopExpr Or e1' e2'
    NewStore _ -> undefined
    Dereference _ -> undefined

verify :: Opts -> Program -> IO Bool
verify ops program = do
    tStart <- getCPUTime
    -- Get program paths
    let
        tree = limitDepth maxDepth $ programTree program
        paths = flatten tree
    let
        preds = map (\p -> (getTypes p, p)) paths
    -- Check each path
    results <- traverse (uncurry checkValid) preds
    tEnd <- getCPUTime
    undefined
  where
    Opts { maxDepth } = ops

flatten :: PathTree -> [Expr]
flatten = undefined

checkValid :: Map String Type -> Expr -> IO (Maybe String)
checkValid env p =
    (mkEnv env
        -- Convert it to a Z3 AST
        >>= runReaderT (convert p)
        -- Negate it, if there is a model that is not valid this will let us know
        >>= mkNot
        >>= assert)
    -- Run the model
    *> withModel modelToString <&> snd & evalZ3

checkFeasible :: Map String Type -> Expr -> IO Bool
checkFeasible env p =
    (mkEnv env
        -- Convert it to a Z3 AST
        >>= runReaderT (convert p)
        >>= assert)
    -- Run the model
    *> check <&> (== Sat) & evalZ3
