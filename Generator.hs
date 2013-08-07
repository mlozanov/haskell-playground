module Generator where

import Control.Monad

data Generator a = Generator a

instance Functor Generator where
    fmap f (Generator v) = Generator (f v)

instance Monad Generator where
    return v = Generator v
    Generator v >>= f = f v

