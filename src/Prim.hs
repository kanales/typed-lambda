module Prim (prims) where

import           Debug.Trace
import           Eval
import           Syntax
-- WARNING: unsafe
castI :: Term -> Int
castI x = case x of
    (TVal (VInt i)) -> i
    x -> error $ "fatal error, " <> show x <> " can't be cast to int"


castB :: Term -> Bool
castB x = case x of
    (TVal (VBool i)) -> i
    x -> error $ "fatal error, " <> show x <> " can't be cast to bool"


fixType = ((TInt `TArrow` TInt) `TArrow` (TInt `TArrow` TInt)) `TArrow` (TInt `TArrow` TInt)

prims :: [(String, (Type, Term))]
prims =
    [ ( "+prim",   (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x + castI y)))))
    , ( "-prim",   (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x - castI y)))))
    , ( "*prim",   (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x * castI y)))))
    , ( "divPrim", (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x `div` castI y)))))
    , ( "modPrim", (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x `mod` castI y)))))
    , ( "ifPrim",  (TBool `TArrow` (TInt `TArrow` (TInt `TArrow` TInt)),  TPrim (\flag -> TPrim (\t -> TPrim (\f -> if castB flag then t else f)))))
    , ( "eq", (TInt `TArrow` (TInt `TArrow` TBool), TPrim (\x -> TPrim (\y -> TVal (VBool $ castI x == castI y)))))
    , ( "ne", (TInt `TArrow` (TInt `TArrow` TBool), TPrim (\x -> TPrim (\y -> TVal (VBool $ castI x /= castI y)))))
    , ( "le", (TInt `TArrow` (TInt `TArrow` TBool), TPrim (\x -> TPrim (\y -> TVal (VBool $ castI x <= castI y)))))
    , ( "ge", (TInt `TArrow` (TInt `TArrow` TBool), TPrim (\x -> TPrim (\y -> TVal (VBool $ castI x >= castI y)))))
    , ( "gt", (TInt `TArrow` (TInt `TArrow` TBool), TPrim (\x -> TPrim (\y -> TVal (VBool $ castI x > castI y)))))
    , ( "lt", (TInt `TArrow` (TInt `TArrow` TBool), TPrim (\x -> TPrim (\y -> TVal (VBool $ castI x < castI y)))))
    --, ( "fix", (fixType , TClosure (TClosure (TVar 1 `TApp` TVar 1) `TApp` TClosure (TVar 0 `TApp` (TVar 1 `TApp` TVar 1)))))
    ]
