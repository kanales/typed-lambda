module Lib
    ( module Eval
    , module Parser
    , module Pretty
    , module Syntax
    , module Check
    , lib
    , primState
    ) where

import           Check
import           Eval
import           Parser
import           Pretty
import           Prim
import           Syntax


lib :: [Stmt]
lib =
    [ Let "ipair" (Lam "x" TInt (Lam "y" TInt (Lam "f" (TArrow (TArrow TInt TInt) TInt) (App (App (Var "f") (Var "x")) (Var "y")))))
    , Let "ifst" (Lam "p" (TArrow (TArrow TInt TInt) (TArrow (TArrow TInt TInt) TInt)) (App (Var "p") (Lam "x" TInt (Lam "y" TInt (Var "x")))))
    , Let "isnd" (Lam "p" (TArrow (TArrow TInt TInt) (TArrow (TArrow TInt TInt) TInt)) (App (Var "p") (Lam "x" TInt (Lam "y" TInt (Var "y")))))
    , Let "+" (Lam "x" TInt (Lam "y" TInt (App (App (Var "+prim") (Var "x")) (Var "y"))))
    , Let "*" (Lam "x" TInt (Lam "y" TInt (App (App (Var "*prim") (Var "x")) (Var "y"))))
    , Let "-" (Lam "x" TInt (Lam "y" TInt (App (App (Var "-prim") (Var "x")) (Var "y"))))
    , Let "div" (Lam "x" TInt (Lam "y" TInt (App (App (Var "divPrim") (Var "x")) (Var "y"))))
    , Let "mod" (Lam "x" TInt (Lam "y" TInt (App (App (Var "modPrim") (Var "x")) (Var "y"))))
    , Let "if"  (Lam "f" TBool (Lam "x" TInt (Lam "y" TInt (App (App (App (Var "ifPrim") (Var "f")) (Var "x")) (Var "y")))))
    ]

primState :: EvalState
primState = initialState prims

