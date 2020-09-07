{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval
    ( eval
    , subs
    , freeVars
    , EvalT
    , EvalError(..)
    , runEvalT
    , emptyState
    , initialState
    ) where

import Syntax

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except

primitives :: [(String, Expr)]
primitives =
    [ ("add", Prim (\x -> Prim (\y -> Lit $ x + y))) 
    , ("sub", Prim (\x -> Prim (\y -> Lit $ x - y)))
    , ("mul", Prim (\x -> Prim (\y -> Lit $ x * y)))
    , ("mod", Prim (\x -> Prim (\y -> Lit $ x + y)))
    , ("eq",  Prim (\x -> Prim (\y -> if x == y then true else false)))
    , ("ne",  Prim (\x -> Prim (\y -> if x /= y then true else false)))
    ]
    where true  = Lam "x" (Lam "y" (Var "x"))
          false = Lam "x" (Lam "y" (Var "y"))

data EvalError = VariableNotInScope String | TypeMismatch
    deriving (Show)

data EvalState = EvalState { expressionMap :: [(String, Expr)] }

emptyState = EvalState []

initialState = EvalState

newtype EvalT m a = EvalT { runE :: ExceptT EvalError (StateT EvalState m) a }
    deriving (Functor, Applicative, Monad, MonadState EvalState, MonadError EvalError, MonadIO)

instance MonadTrans EvalT where
    lift = EvalT . lift . lift

runEvalT :: Monad m => EvalT m a -> EvalState -> m (Either EvalError a)
runEvalT e = evalStateT (runExceptT (runE e))

-- For a given lambda `Lam y e` return an alpha equivalent
-- lambda `Lam y' e'` such that y is not a free variable of expr
-- standarize :: String -> Expr -> Expr -> Expr
-- standarize y e expr =
--     let fv   = freeVars expr
--         ff x = not $ x `elem` fv
--         free = fromJust $ find ff (y: [y <> show i | i <- [1..]])
--     in Lam free (subs y (Var free) e)

subs :: String -> Expr -> Expr -> Expr
subs key val expr = case expr of
    Lam arg body | arg /= key -> 
        let 
            fv      = freeVars val
            ff x    = not $ x `elem` fv
            newArg  = fromJust $ find ff (arg: [arg <> show i | i <- [1..]])
            newBody = subs arg (Var newArg) body
        in  Lam newArg (ss newBody)
    Lam arg body | arg == key -> Lam arg body
    App left right            -> App (ss left) (ss right)
    Var arg      | arg == key -> val
    _                         -> expr
    where ss = subs key val

freeVars :: Expr -> [String]
freeVars e = case e of
    Var k -> [k]
    App l r -> freeVars l ++ freeVars r
    Lam arg body -> filter (/= arg) (freeVars body)
    _ -> []


extend :: Monad m => String -> Expr -> EvalT m ()
extend s e = do
    env <- gets expressionMap
    put $ EvalState  ( (s,e) : env )

lookupExpr :: Monad m => String -> EvalT m Expr
lookupExpr s = do
    env <- gets expressionMap
    case lookup s env <|> lookup s primitives of
        Nothing -> throwError (VariableNotInScope s)
        Just r  -> return r

performSubs :: Monad m => [String] -> Expr -> EvalT m Expr
performSubs [] e = return e
performSubs (v:vs) e = do
    val <- lookupExpr v
    performSubs vs (subs v val e)

evalApp :: Monad m => Expr -> Expr -> EvalT m Expr
evalApp left right = do
    left <- evalExpr left
    right <- evalExpr right
    case left of
        Lam k v -> evalExpr $ subs k right v
        Prim f  -> evalPrim f right
        _       -> return $ App left right

evalExpr :: Monad m => Expr -> EvalT m Expr
evalExpr e = do
    let fvs = nub $ freeVars e
    newE <- performSubs fvs e

    case newE of
        App l r -> evalApp l r
        _       -> return newE
    
evalPrim :: Monad m => (Int -> Expr) -> Expr -> EvalT m Expr
evalPrim f e = case e of
    Lit x -> return (f x)
    _     -> throwError TypeMismatch

eval :: Monad m => Stmt -> EvalT m Expr
eval (Let v x) = do
    extend v x 
    env <- gets expressionMap
    return x
eval (Expr e)  = evalExpr e