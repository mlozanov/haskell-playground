{-# LANGUAGE ForeignFunctionInterface #-}

module TestFFI where

import Foreign
import Foreign.C

testFFI :: Int -> IO ()
testFFI = testFFI_h

foreign import ccall "testFFI_c" testFFI_h :: Int -> IO ()