module Main where

import Lib
import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.Except

process :: String -> EvalT (InputT IO) ()
process line = do
    case parse stmt line of
        Left err -> lift $ outputStrLn (show err)
        Right s -> do
            ex <- eval s
            lift $ outputStrLn (show $ pp ex)

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


main :: IO ()
main = do
    ev <- runInputT defaultSettings (runEvalT program emptyState)
    case ev of
        Left ex -> putStrLn $ "Error: " <> show ex
        Right r -> return r

