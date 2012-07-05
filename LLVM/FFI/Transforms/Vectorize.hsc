{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.Transforms.Vectorize(
       addBBVectorizePass
       ) where

import LLVM.FFI.Core

foreign import ccall unsafe "LLVMAddBBVectorizePass"
  addBBVectorizePass :: PassManagerRef -> IO ()
