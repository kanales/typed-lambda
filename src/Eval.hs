{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval
    ( eval
    , Value(..)
    , Term(..)
    , freeVars
    , evalStmt
    , EvalT
    , EvalError(..)
    , runEvalT
    , emptyState
    , initialState
    ) where

import Syntax
import Pretty

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except

-- VALUES
data Value
    = VInt Int
    | VBool Bool
    deriving (Show)

instance Pretty Value where
    ppr _ v = case v of
        VInt i  -> text (show i)
        VBool b -> text (show b)

data Term
    = TClosure Term
    | TVar Int
    | TApp Term Term
    | TVal Value
    deriving (Show)

instance Pretty Term where
    ppr d v = case v of
        TClosure _  -> parensIf (d>0) (text "<<closure>>")
        TVar _      -> undefined -- text ("$" <> show i)
        TApp _ _    -> undefined -- parensIf (d>0) (ppr (d+1) l <+> ppr (d+1) r)
        TVal v      -> pp v

substitute :: Term -> Term -> Term
substitute val into = case into of
    TClosure v -> TClosure $ substitute val v
    TApp l r   -> TApp (substitute val l) (substitute val r)
    TVar     0 -> val
    TVar d     -> TVar (d-1)
    TVal v     -> TVal v

shift :: Int -> Term -> Term
shift by term = case term of
    TClosure v -> TClosure $ shift by v
    TApp l r   -> TApp (shift by l) (shift by r)
    TVar d     -> TVar (d+by)
    TVal v     -> TVal v

-- EVAL
data EvalError = VariableNotInScope String | TypeMismatch | NotAClosure Term
    deriving (Show)

newtype EvalState = EvalState { environment :: [(String, Term)] }

emptyState ::  EvalState
emptyState   = EvalState []

initialState :: [(String, Term)] -> EvalState
initialState = EvalState

newtype EvalT m a = EvalT { runE :: ExceptT EvalError (StateT EvalState m) a }
    deriving (Functor, Applicative, Monad, MonadState EvalState, MonadError EvalError, MonadIO)

instance MonadTrans EvalT where
    lift = EvalT . lift . lift

runEvalT :: Monad m => EvalT m a -> EvalState -> m (Either EvalError a)
runEvalT e = evalStateT (runExceptT (runE e))

extend :: Monad m => String -> Term -> EvalT m ()
extend s e = do
    env <- gets environment
    put $ EvalState ( (s,e) : env )

envLookup :: Monad m => String -> EvalT m Term
envLookup s = do
    env <- gets environment
    case lookup s env of
        Nothing -> throwError (VariableNotInScope s)
        Just r  -> return r

eval :: Monad m => Term -> EvalT m Term
eval v = case v of
    TApp l r -> do 
        left  <- eval l
        case left of
            TClosure t -> eval $ substitute r t
            _          -> TApp left <$> eval r
    TClosure e -> TClosure <$> eval e
    _          -> return v
    
toTerm :: Monad m => Expr -> EvalT m Term
toTerm = toTerm' 0 [] where
    toTerm' :: Monad m => Int -> [(String, Int)] -> Expr -> EvalT m Term
    toTerm' d bound v = case v of
        Var s -> do
            case lookup s bound of
                Nothing -> shift d <$> envLookup s
                Just  p -> return $ TVar p
        App left right -> do
            l <- toTerm' d bound left
            r <- toTerm' d bound right
            return $ TApp l r
        Lam arg body -> TClosure <$> toTerm' (d+1) ((arg,d) : bound) body
        Lit lit -> return $ case lit of
            LInt i  -> TVal (VInt i)
            LBool b -> TVal (VBool b)


evalStmt :: Monad m => Stmt -> EvalT m Term
evalStmt s = case s of
    Let binding e -> do
        t <- handle e
        extend binding t
        return t
    Expr e -> handle e
    where 
        handle e = do
            t <- toTerm e >>= eval
            case t of
                -- Could not reduce
                TApp l _ -> throwError $ NotAClosure l
                other    -> return other

freeVars :: Expr -> [String]
freeVars e = case e of
    Var k -> [k]
    App l r -> freeVars l ++ freeVars r
    Lam arg body -> filter (/= arg) (freeVars body)
    _ -> []
