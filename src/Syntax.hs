module Syntax 
    ( Name
    , Expr(..)
    , Stmt(..)
    , Literal(..)
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
    | Lit Literal
    deriving (Show, Eq)

data Literal
    = LInt Int
    | LBool Bool
    deriving (Show, Eq)

data Type
    = TBool
    | TInt
    | TArrow Type Type
    deriving (Show, Eq)

instance Pretty Expr where
    ppr p e = case e of
        Lit i   -> text $ show i
        Var x   -> text x
        App a b -> parensIf (p>0) $ ppr (p+1) a <+> ppr p b
        Lam x a -> parensIf (p>0) $ char '\\' <> hsep (pp <$> viewVars e) <> text " . " <> ppr (p+1) (viewBody e)
            where
                viewVars :: Expr -> [Name]
                viewVars (Lam n a) = n : viewVars a
                viewVars _ = []

                viewBody :: Expr -> Expr
                viewBody (Lam _ a) = viewBody a
                viewBody x = x
        --Prim _ -> text "<<primitive>>"

data Stmt
    = Let String Expr
    | Expr Expr
    deriving (Show)