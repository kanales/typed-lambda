{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
module Eval
    ( eval
    , Value(..)
    , Term(..)
    , freeVars
    , getExpression
    , evalStmt
    , EvalT
    , EvalError(..)
    , runEvalT
    , emptyState
    , initialState
    , toTerm
    , EvalState(..)
    ) where

import           Check
import           Pretty
import           Syntax

import           Control.Monad.Except
import           Control.Monad.State
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
            TClosure c -> showString "TClosure " . showsPrec (appPrec+1) c
            TPrim _    -> showString "TPrim (?)"
            TVal v     -> showString "TVal " . showsPrec (appPrec+1) v
            TVar i     -> showString "TVar "  . showsPrec (appPrec+1) i
            TApp l r   -> showString "TApp " . showsPrec (appPrec+1) l . showChar ' ' . showsPrec (appPrec+1) r
            --Fix  f     -> showString "fix" . showsPrec (appPrec+1) f
            where appPrec = 10

instance Pretty Term where
    ppr d v = case v of
        TClosure _ -> parensIf (d>0) (text "<<closure>>")
        TPrim    _ -> parensIf (d>0) (text "<<prim>>")
        TVar x     -> char '$' <> text (show x)
        TApp l r   -> parensIf (d>0) (ppr (d+1) l <+> ppr (d+1) r)
        TVal x     -> pp x
        --Fix  f     -> text "fix" <+> ppr (d+1) f

substitute :: Term -> Term -> Term
substitute val into = case into of
    TClosure v -> TClosure $ substitute val v
    TApp l r   -> TApp (substitute val l) (substitute val r)
    TVar     0 -> val
    TVar d     -> TVar (d-1)
    TVal v     -> TVal v
    TPrim _    -> into
    --Fix   f    -> Fix (substitute val f)

shift :: Int -> Term -> Term
shift by term = case term of
    TClosure v -> TClosure $ shift by v
    TApp l r   -> TApp (shift by l) (shift by r)
    TVar d     -> TVar (d+by)
    TVal v     -> TVal v
    TPrim _    -> term
    --Fix   f    -> Fix (shift by f)

-- EVAL
data EvalError = VariableNotInScope String | NotAValue Term | NotAClosure Term | TypeError TypeError
    deriving (Show)

newtype EvalState = EvalState { environment :: [(String, (Type, Term))] }

emptyState ::  EvalState
emptyState   = EvalState { environment = [] }

initialState ::  [(String, (Type, Term))] -> EvalState
initialState prims = EvalState { environment = prims }

newtype EvalT m a = EvalT { runE :: ExceptT EvalError (StateT EvalState m) a }
    deriving (Functor, Applicative, Monad, MonadState EvalState, MonadError EvalError, MonadIO)

instance MonadTrans EvalT where
    lift = EvalT . lift . lift

instance MonadCheck m => MonadCheck (EvalT m) where
    check = lift . check

runEvalT :: Monad m => EvalT m a -> EvalState -> m (Either EvalError a)
runEvalT e = evalStateT (runExceptT (runE e))

extend :: Monad m => String -> (Type, Term) -> EvalT m ()
extend s (t,e) = do
    eState <- get
    let env = environment eState
    put $ eState { environment = (s,(t,e)) : env }

lookupTerm :: Monad m => String -> EvalT m Term
lookupTerm s = do
    env <- gets environment
    case snd <$> lookup s env of
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
    _                   -> return v


toTerm' :: Monad m => Int -> [(String, Int)] -> Expr -> EvalT m Term
toTerm' d bound v = case v of
    Var s -> do
        case lookup s bound of
            Nothing -> shift d  <$> lookupTerm s
            Just  p -> return $ TVar p
    App left right -> do
        l <- toTerm' d bound left
        r <- toTerm' d bound right
        return $ TApp l r
    Lam arg _ body -> TClosure <$> toTerm' (d+1) ((arg,d) : bound) body
    Lit lit -> return $ case lit of
        LInt i  -> TVal (VInt i)
        LBool b -> TVal (VBool b)

toTerm :: Monad m => Expr -> EvalT m (Type, Term)
toTerm e = do
    env <- gets (types . environment)
    res <- runCheckT (check e) env
    ty <- case res of
        Right t  ->  return t
        Left  er ->  throwError $ TypeError er

    t  <- toTerm' 0 [] e
    return (ty, t)
    where
        types :: [(String, (Type, Term))] -> [(String, Type)]
        types = fmap $ \(x, (ty, _)) -> (x, ty)

toTermNamed :: Monad m => String -> Expr -> EvalT m (Type, Term)
toTermNamed name e = do
    res <- toTerm e
    extend name res
    return res

getExpression :: Stmt -> Expr
getExpression (Expr e)  = e
getExpression (Let _ e) = e

evalStmt :: Monad m => Stmt -> EvalT m (Type, Term)
evalStmt s = case s of
    Let binding e -> toTermNamed binding e
    Expr e -> do
        (ty, te) <- toTerm e
        (ty,) <$> eval te

freeVars :: Expr -> [String]
freeVars e = case e of
    Var k          -> [k]
    App l r        -> freeVars l ++ freeVars r
    Lam arg _ body -> filter (/= arg) (freeVars body)
    _              -> []
