{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval
    ( eval
    , subs
    , freeVars
    , EvalT
    , EvalError(..)
    , runEvalT
    , emptyState
    ) where

import Syntax

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except

data EvalError = VariableNotInScope String
    deriving (Show)

data EvalState = EvalState { expressionMap :: [(String, Expr)] }

emptyState = EvalState []

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
    Lit _ -> []
    Var k -> [k]
    App l r -> freeVars l ++ freeVars r
    Lam arg body -> filter (/= arg) (freeVars body)


extend :: Monad m => String -> Expr -> EvalT m ()
extend s e = do
    env <- gets expressionMap
    put $ EvalState  ( (s,e) : env )

lookupExpr :: Monad m => String -> EvalT m Expr
lookupExpr s = do
    env <- gets expressionMap
    case lookup s env of
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
        _       -> return $ App left right

evalExpr :: Monad m => Expr -> EvalT m Expr
evalExpr e = do
    let fvs = nub $ freeVars e
    newE <- performSubs fvs e

    case newE of
        App l r -> evalApp l r
        _       -> return newE
    
eval :: Monad m => Stmt -> EvalT m Expr
eval (Let v x) = do
    extend v x 
    env <- gets expressionMap
    return x
eval (Expr e)  = evalExpr e