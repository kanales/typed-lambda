{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Check
    ( CheckT
    , runCheckT
    , check
    , TypeError
    , MonadCheck
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Syntax


type Env = [(String, Type)]
data TypeError
    = TypeMismatch Type Type
    | NotFunctionType Type
    | NotInScope String
    deriving (Show, Eq)

newtype CheckT m a = C { runC :: ExceptT TypeError (ReaderT Env m) a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError TypeError, MonadIO)

instance MonadTrans CheckT where
    lift = C . lift . lift

runCheckT :: Monad m => CheckT m a -> [(String, Type)] -> m (Either TypeError a)
runCheckT c = runReaderT (runExceptT (runC c))

class Monad m => MonadCheck m where
    check :: Expr -> m Type

instance Monad m => MonadCheck (CheckT m) where
    check expr = case expr of
        Lit LInt{}  -> return TInt
        Lit LBool{} -> return TBool
        Lam x t e   -> TArrow t <$> local ((x,t):) (check e)
        App e1 e2   -> do
            t1 <- check e1
            t2 <- check e2
            case t1 of
                (TArrow a b) | a == t2    -> return b
                            | otherwise  -> throwError $ TypeMismatch a t2
                ty -> throwError $ NotFunctionType ty
        Var x       -> do
            env <- ask
            case lookup x env of
                Just e  -> return e
                Nothing -> throwError (NotInScope x)

