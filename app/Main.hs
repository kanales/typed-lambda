module Main where

import Lib
import System.Console.Haskeline
import Control.Monad.Trans

process :: String -> IO ()
process line = do
    case parse expr line of
        Left err -> print err
        Right ex -> print $ pp ex

main :: IO ()
main = runInputT defaultSettings loop
    where loop = do
            input <- getInputLine "untyped> "
            case input of 
                Nothing -> outputStrLn "Goodbye."
                Just input -> (liftIO $ process input) >> loop

