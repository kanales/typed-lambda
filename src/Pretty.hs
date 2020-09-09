{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Pretty 
    ( Pretty(..)
    , parensIf
    , (<+>) 
    , hsep
    , char
    , text
    , sep
    , punctuate
    , Doc
    ) where

import Text.PrettyPrint

class Pretty p where
    ppr :: Int -> p -> Doc

    pp  :: p -> Doc
    pp = ppr 0

instance Pretty String where
    ppr _ = text

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id