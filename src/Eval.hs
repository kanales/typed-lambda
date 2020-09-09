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

import Debug.Trace
-- VALUES
newtype Value
    = VInt Int
    deriving (Show)

instance Pretty Value where
    ppr _ v = case v of
        VInt i -> text (show i)

data Term
    = TClosure Term
    | TVar Int
    | TApp Term Term
    | TVal Value
    deriving (Show)

instance Pretty Term where
    ppr d v = case v of
        TClosure _  -> parensIf (d>0) (text "<<closure>>")
        TVar i      -> undefined -- text ("$" <> show i)
        TApp l r    -> undefined -- parensIf (d>0) (ppr (d+1) l <+> ppr (d+1) r)
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
        Lit i -> return $ TVal (VInt i)


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
-- eval (Let v x) = do
--     extend v x 
--     env <- gets expressionMap
--     return x
-- eval (Expr e)  = evalExpr e
-- primitives :: [(String, Expr)]
-- primitives =
--     [ ("add", Prim (\x -> Prim (\y -> Lit $ x + y))) 
--     , ("sub", Prim (\x -> Prim (\y -> Lit $ x - y)))
--     , ("mul", Prim (\x -> Prim (\y -> Lit $ x * y)))
--     , ("mod", Prim (\x -> Prim (\y -> Lit $ x + y)))
--     , ("eq",  Prim (\x -> Prim (\y -> if x == y then true else false)))
--     , ("ne",  Prim (\x -> Prim (\y -> if x /= y then true else false)))
--     ]
--     where true  = Lam "x" (Lam "y" (Var "x"))
--           false = Lam "x" (Lam "y" (Var "y"))



-- For a given lambda `Lam y e` return an alpha equivalent
-- lambda `Lam y' e'` such that y is not a free variable of expr
-- standarize :: String -> Expr -> Expr -> Expr
-- standarize y e expr =
--     let fv   = freeVars expr
--         ff x = not $ x `elem` fv
--         free = fromJust $ find ff (y: [y <> show i | i <- [1..]])
--     in Lam free (subs y (Var free) e)

-- subs :: String -> Expr -> Expr -> Expr
-- subs key val expr = case expr of
--     Lam arg body | arg /= key -> 
--         let 
--             fv      = freeVars val
--             ff x    = not $ x `elem` fv
--             newArg  = fromJust $ find ff (arg: [arg <> show i | i <- [1..]])
--             newBody = subs arg (Var newArg) body
--         in  Lam newArg (ss newBody)
--     Lam arg body | arg == key -> Lam arg body
--     App left right            -> App (ss left) (ss right)
--     Var arg      | arg == key -> val
--     _                         -> expr
--     where ss = subs key val

freeVars :: Expr -> [String]
freeVars e = case e of
    Var k -> [k]
    App l r -> freeVars l ++ freeVars r
    Lam arg body -> filter (/= arg) (freeVars body)
    _ -> []

-- performSubs :: Monad m => [String] -> Expr -> EvalT m Expr
-- performSubs [] e = return e
-- performSubs (v:vs) e = do
--     val <- lookupExpr v
--     performSubs vs (subs v val e)

-- evalApp :: Monad m => Expr -> Expr -> EvalT m Expr
-- evalApp left right = do
--     left <- evalExpr left
--     right <- evalExpr right
--     case left of
--         Lam k v -> evalExpr $ subs k right v
--         Prim f  -> evalPrim f right
--         _       -> return $ App left right

-- evalExpr :: Monad m => Expr -> EvalT m Value
-- evalExpr (Lam arg body)   = undefined
--     where
-- evalExpr (App left right) = undefined
-- evalExpr (Lit i)          = VLit i
-- evalExpr  = do
--     let fvs = nub $ freeVars e
--     newE <- performSubs fvs e

--     case newE of
--         App l r -> evalApp l r
--         _       -> return newE
    
-- evalPrim :: Monad m => (Int -> Expr) -> Expr -> EvalT m Expr
-- evalPrim f e = case e of
--     Lit x -> return (f x)
--     _     -> throwError TypeMismatch

-- eval :: Monad m => Stmt -> EvalT m Expr
-- eval (Let v x) = do
--     extend v x 
--     env <- gets expressionMap
--     return x
-- eval (Expr e)  = evalExpr e