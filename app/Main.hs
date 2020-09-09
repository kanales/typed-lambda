module Main where

import Lib

import Data.Either
import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.Except

process :: String -> EvalT (InputT IO) ()
process line = do
    case parse stmt line of
        Left err -> lift $ outputStrLn (show err)
        Right s -> do
            val <- evalStmt s
            lift $ outputStrLn (show val)
            lift $ outputStrLn (show $ pp val)

program :: EvalT (InputT IO) ()
program = loop
    where 
        loop = do
            input <- lift $ getInputLine "untyped> "
            case input of
                Nothing -> lift $ outputStrLn "Goodbye."
                Just input -> process input `catchError` handler >> loop
            where 
                handler :: EvalError -> EvalT (InputT IO) ()
                handler = liftIO . print

stdlib :: Monad m => EvalT m ()
stdlib =  
    let stmts :: [Stmt]
        stmts = fromRight undefined . sequence $ parse stmt <$>
            [ "let true  = \\x y. x"
            , "let false = \\x y. y"
            , "let pair  = \\x y f. f x y"
            , "let fst   = \\p. p true"
            , "let snd   = \\p. p false"
            , "let fix   = \\g. (\\x. g(x x)) (\\x. g (x x))"
            , "let nil   = \\x. true"
            , "let null  = \\p. p (\\x y. false)"
            ]
    in mapM_ evalStmt stmts

main :: IO ()
main = do
    ev <- runInputT defaultSettings (runEvalT (stdlib >> program) emptyState)
    case ev of
        Left ex -> putStrLn $ "Error: " <> show ex
        Right r -> return r

