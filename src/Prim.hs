module Prim (prims) where

import           Debug.Trace
import           Eval
-- WARNING: unsafe
castI :: Term -> Int
castI x = case x of
    (TVal (VInt i)) -> i
    x -> error $ "fatal error, " <> show x <> " can't be cast to int"


castB :: Term -> Bool
castB x = case x of
    (TVal (VBool i)) -> i
    x -> error $ "fatal error, " <> show x <> " can't be cast to bool"

prims :: [(String, Term)]
prims =
    [ ( "+prim", TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x + castI y))))
    , ( "-prim", TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x - castI y))))
    , ( "*prim", TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x * castI y))))
    , ( "divPrim", TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x `div` castI y))))
    , ( "modPrim", TPrim (\x -> TPrim (\y -> TVal (VInt $ castI x `mod` castI y))))
    , ( "ifPrim",  TPrim (\flag -> TPrim (\t -> TPrim (\f -> if castB flag then t else f))))
    ]
