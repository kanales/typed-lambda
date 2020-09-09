module Main where

import           Lib

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans
import           Data.Either
import           System.Console.Haskeline

process :: String -> EvalT (InputT IO) ()
process line = do
    case parse stmt line of
        Left err -> lift $ outputStrLn (show err)
        Right s -> do
            val <- evalStmt s
            lift $ outputStrLn (show val)
            lift $ outputStrLn (show $ pp val)

program :: EvalT (InputT IO) ()
program = loadLib >> loop
    where
        loadLib = forM_ lib evalStmt

        loop = do
            input <- lift $ getInputLine "untyped> "
            case input of
                Nothing    -> lift $ outputStrLn "Goodbye."
                Just input -> process input `catchError` handler >> loop
            where
                handler :: EvalError -> EvalT (InputT IO) ()
                handler = liftIO . print

main :: IO ()
main = do
    ev <- runInputT defaultSettings (runEvalT program primState)
    case ev of
        Left ex -> putStrLn $ "Error: " <> show ex
        Right r -> return r

