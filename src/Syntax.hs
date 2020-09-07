module Syntax 
    ( Name
    , Expr(..)
    , Stmt(..)
    ) where

import Pretty

type Name = String

-- data Lit
--     = LInt Int
--     | LBool Bool
--     deriving (Show)

data Expr
    = Var Name
    | App Expr Expr
    | Lam Name Expr
    | Lit Int
    deriving (Show, Eq)

instance Pretty Expr where
    ppr p e = case e of
        Lit i   -> text $ show i
        Var x   -> text x
        App a b -> parensIf (p>0) $ ppr (p+1) a <+> ppr p b
        Lam x a -> parensIf (p>0) $ char '\\' <> hsep (fmap pp $ viewVars e) <> text " . " <> ppr (p+1) (viewBody e)
            where
                viewVars :: Expr -> [Name]
                viewVars (Lam n a) = n : viewVars a
                viewVars _ = []

                viewBody :: Expr -> Expr
                viewBody (Lam _ a) = viewBody a
                viewBody x = x

data Stmt
    = Let String Expr
    | Eval Expr
    deriving (Show)