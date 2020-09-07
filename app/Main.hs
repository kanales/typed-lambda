module Main where

import Lib
import System.Console.Haskeline
import Control.Monad.Trans

process :: String -> IO ()
process line = do
    case parse stmt line of
        Left err -> print err
        Right ex -> case ex of
            Let i x -> putStrLn $ i <> " = " <> show x
            Eval  x -> print . pp $ eval x

main :: IO ()
main = runInputT defaultSettings loop
    where loop = do
            input <- getInputLine "untyped> "
            case input of 
                Nothing -> outputStrLn "Goodbye."
                Just input -> (liftIO $ process input) >> loop

