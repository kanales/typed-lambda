module Eval
    ( eval
    , subs
    ) where

import Syntax

subs :: String -> Expr -> Expr -> Expr
subs key val expr = case expr of
    Lam arg body | arg /= key -> Lam arg (subs key val body)
    App left right            -> App (subs key val left) (subs key val right)
    Var key                   -> val
    _                         -> expr

eval :: Expr -> Expr
eval e = case e of
    {- perform beta reduction -}
    App (Lam arg body) right -> subs arg body right
    _                        -> e