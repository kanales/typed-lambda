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
    , toTerm
    , EvalState
    ) where

import           Pretty
import           Syntax

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans

import           Debug.Trace
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
    | TPrim (Term -> Term)
    | TVar Int
    | TApp Term Term
    | TVal Value

instance Show Term where
    showsPrec d t = showParen (d>appPrec) $ case t of
            TClosure t -> showString "TClosure " . showsPrec (appPrec+1) t
            TPrim p    -> showString "TPrim (?)"
            TVal v     -> showString "TVal " . showsPrec (appPrec+1) v
            TVar i     -> showString "TVar "  . showsPrec (appPrec+1) i
            TApp l r   -> showString "TApp " . showsPrec (appPrec+1) l . showChar ' ' . showsPrec (appPrec+1) r
            where appPrec = 10

instance Pretty Term where
    ppr d v = case v of
        TClosure _ -> parensIf (d>0) (text "<<closure>>")
        TPrim    _ -> parensIf (d>0) (text "<<prim>>")
        TVar v     -> char '$' <> text (show v)
        TApp l r   -> parensIf (d>0) (ppr (d+1) l <+> ppr (d+1) r)
        TVal v     -> pp v

substitute :: Term -> Term -> Term
substitute val into = case into of
    TClosure v -> TClosure $ substitute val v
    TApp l r   -> TApp (substitute val l) (substitute val r)
    TVar     0 -> val
    TVar d     -> TVar (d-1)
    TVal v     -> TVal v
    TPrim _    -> into

shift :: Int -> Term -> Term
shift by term = case term of
    TClosure v -> TClosure $ shift by v
    TApp l r   -> TApp (shift by l) (shift by r)
    TVar d     -> TVar (d+by)
    TVal v     -> TVal v
    TPrim _    -> term

-- EVAL
data EvalError = VariableNotInScope String | NotAValue Term | NotAClosure Term
    deriving (Show)

data EvalState = EvalState { environment :: [(String, Term)], primitives :: [(String, Term)] }

emptyState ::  EvalState
emptyState   = EvalState { environment = [], primitives = [] }

initialState ::  [(String, Term)] -> EvalState
initialState prims = EvalState { environment = [], primitives = prims}

newtype EvalT m a = EvalT { runE :: ExceptT EvalError (StateT EvalState m) a }
    deriving (Functor, Applicative, Monad, MonadState EvalState, MonadError EvalError, MonadIO)

instance MonadTrans EvalT where
    lift = EvalT . lift . lift

runEvalT :: Monad m => EvalT m a -> EvalState -> m (Either EvalError a)
runEvalT e = evalStateT (runExceptT (runE e))

extend :: Monad m => String -> Term -> EvalT m ()
extend s e = do
    state <- get
    let env = environment state
    put $ state { environment = (s,e) : env }

envLookup :: Monad m => String -> EvalT m Term
envLookup s = do
    env <- gets environment
    prim <- gets primitives
    case lookup s prim <|> lookup s env of
        Nothing -> throwError (VariableNotInScope s)
        Just r  -> return r

eval :: Monad m => Term -> EvalT m Term
eval v = case v of
    TApp (TClosure l) r -> eval $ substitute r l
    TApp (TPrim    f) r -> (f <$> eval r) >>= eval
    TApp l@TApp {}  r   -> do
        left <- eval l
        eval (TApp left r)
    TApp l _            -> throwError $ NotAClosure l
    TApp l            r -> TApp <$> eval l <*> eval r
    _                   -> return v

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
        Lam arg _ body -> TClosure <$> toTerm' (d+1) ((arg,d) : bound) body
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
        handle e = toTerm e >>= eval

freeVars :: Expr -> [String]
freeVars e = case e of
    Var k          -> [k]
    App l r        -> freeVars l ++ freeVars r
    Lam arg _ body -> filter (/= arg) (freeVars body)
    _              -> []
