{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.BitWriter(
       writeBitcodeToFile,
       writeBitcodeToFD
       ) where

import Foreign.C.String(CString)
import Foreign.C.Types(CInt(..))
import Foreign.C.Types(CInt)

import LLVM.FFI.Core

foreign import ccall unsafe "LLVMWriteBitcodeToFile"
  writeBitcodeToFile :: ModuleRef -> CString -> IO CInt

foreign import ccall unsafe "LLVMWriteBitcodeToFD"
  writeBitcodeToFD :: ModuleRef -> CInt -> CInt -> CInt -> IO CInt
