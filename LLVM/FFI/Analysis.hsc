{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.Analysis(
       VerifierFailureAction(..),
       fromVerifierFailureAction,
       toVerifierFailureAction,

       verifyModule,
       verifyFunction,
       viewFunctionCFG,
       viewFunctionCFGOnly
       ) where

import Foreign.C.String(CString)
import Foreign.C.Types(CInt(..))
import Foreign.Ptr(Ptr)

import LLVM.FFI.Core

#include <llvm-c/Analysis.h>

data VerifierFailureAction =
    AbortProcess
  | PrintMessage
  | ReturnStatus

fromVerifierFailureAction :: VerifierFailureAction -> CInt
fromVerifierFailureAction AbortProcess = (#const LLVMAbortProcessAction)
fromVerifierFailureAction PrintMessage = (#const LLVMPrintMessageAction)
fromVerifierFailureAction ReturnStatus = (#const LLVMReturnStatusAction)

toVerifierFailureAction :: CInt -> VerifierFailureAction
toVerifierFailureAction c
  | c == (#const LLVMAbortProcessAction) = AbortProcess
  | c == (#const LLVMPrintMessageAction) = PrintMessage
  | c == (#const LLVMReturnStatusAction) = ReturnStatus

foreign import ccall unsafe "LLVMVerifyModule"
  verifyModule :: ModuleRef -> CInt -> (Ptr CString) -> IO CInt

foreign import ccall unsafe "LLVMVerifyFunction"
  verifyFunction :: ValueRef -> CInt -> IO CInt

foreign import ccall unsafe "LLVMViewFunctionCFG"
  viewFunctionCFG :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMViewFunctionCFGOnly"
  viewFunctionCFGOnly :: ValueRef -> IO ()
