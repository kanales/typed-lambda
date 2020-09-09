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

prims :: [(String, (Type, Term))]
prims =
    [ ( "+prim",   (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x + castI y)))))
    , ( "-prim",   (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x - castI y)))))
    , ( "*prim",   (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x * castI y)))))
    , ( "divPrim", (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x `div` castI y)))))
    , ( "modPrim", (TInt `TArrow` (TInt `TArrow` TInt), TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x `mod` castI y)))))
    , ( "ifPrim",  (TBool `TArrow` (TInt `TArrow` (TInt `TArrow` TInt)),  TPrim (\flag -> TPrim (\t -> TPrim (\f -> if castB flag then t else f)))))
    ]
