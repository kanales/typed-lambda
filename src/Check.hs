module Check
    (
    ) where

import Control.Monad.Except

data TypeError

newtype Check a = C { runC :: Except TypeError a }

runCheck :: Check a -> Either TypeError a
runCheck = runExcept . runC