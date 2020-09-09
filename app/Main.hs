module Main where

import           Lib

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           System.Console.Haskeline hiding (getInputLine, outputStrLn)
import qualified System.Console.Haskeline as H

type App = EvalT (InputT IO)

outputStrLn :: String -> App ()
outputStrLn  = lift . H.outputStrLn

getInputLine :: String -> App (Maybe String)
getInputLine = lift . H.getInputLine

runApp :: App a -> EvalState -> IO (Either EvalError a)
runApp p s = runInputT defaultSettings (runEvalT p s)

process :: String -> App ()
process line = do
    case parse stmt line of
        Left err -> outputStrLn (show err)
        Right s -> do
            (ty, val) <- evalStmt s
            outputStrLn (show (pp val) <> " : " <> show (pp ty))

program :: App ()
program = loadLib >> loop
    where
        loadLib = forM_ lib evalStmt

        loop = do
            input <- getInputLine "untyped> "
            case input of
                Nothing    -> outputStrLn "Goodbye."
                Just input -> process input `catchError` handler >> loop
            where
                handler = liftIO . print

main :: IO ()
main = do
    ev <- runApp program primState
    case ev of
        Left ex -> putStrLn $ "Error: " <> show ex
        Right e -> return e
