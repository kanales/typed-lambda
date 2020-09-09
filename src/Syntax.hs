module Syntax
    ( Name
    , Expr(..)
    , Stmt(..)
    , Literal(..)
    , Type(..)
    ) where

import           Pretty

type Name = String

-- data Lit
--     = LInt Int
--     | LBool Bool
--     deriving (Show)

data Expr
    = Var Name
    | App Expr Expr
    | Lam Name Type Expr
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

instance Pretty Type where
    ppr _ TBool        = text "Bool"
    ppr _ TInt         = text "Int"
    ppr d (TArrow l r) = parensIf (d>0) (ppr (d+1) l <+> text "->" <+> ppr (d+1) r)

instance Pretty Expr where
    ppr p e = case e of
        Lit i   -> text $ show i
        Var x   -> text x
        App a b -> parensIf (p>0) $ ppr (p+1) a <+> ppr p b
        Lam x t a -> parensIf (p>0) $ char '\\' <> args e <> text " . " <> ppr (p+1) (viewBody e)
            where
                args :: Expr -> Doc
                args e = hsep $ punctuate (char ',') (viewVars e)

                viewVars :: Expr -> [Doc]
                viewVars (Lam n t a) = (pp n <+> text ":" <+> pp t) : viewVars a
                viewVars _ = []

                viewBody :: Expr -> Expr
                viewBody (Lam _ _ a) = viewBody a
                viewBody x           = x
        --Prim _ -> text "<<primitive>>"

data Stmt
    = Let String Expr
    | Expr Expr
    deriving (Show)

instance Pretty Stmt where
    ppr _ (Let b e) = text "let" <+> pp b <+> pp e
    ppr _ (Expr e)  = pp e
