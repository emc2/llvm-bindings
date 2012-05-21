{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
-- Copyright (c) 2012 Eric McCorkle.  All rights reserved.
--
-- Portions of this code are derived from software originally written
-- by Brian O' Sullivan.  Comments are copied in part from software
-- produced by the LLVM Compiler Infrastructure project.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

-- | Raw FFI bindings for llvm-c/Analysis.h
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
-- | Verifier will print to stderr and abort()
    AbortProcess
-- | Verifier will print to stderr and return 1
  | PrintMessage
-- | Verifier will return 1
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

-- | Verifies that a module is valid, taking the specified action if
-- not.  Optionally returns a human-readable description of any
-- invalid constructs.  Message must be disposed with
-- @disposeMessage@.
foreign import ccall unsafe "LLVMVerifyModule"
  verifyModule :: ModuleRef
               -- ^ Module
               -> CInt
               -- ^ Verifier action
               -> (Ptr CString)
               -- ^ Pointer to error string stored here
               -> IO CInt
               -- ^ 0 if successful

-- | Verifies that a single function is valid, taking the specified
-- action. Useful for debugging.
foreign import ccall unsafe "LLVMVerifyFunction"
  verifyFunction :: ValueRef
                 -- ^ Function
                 -> CInt
                 -- ^ Verifier action
                 -> IO CInt
                 -- ^ 0 if successful

-- | Open up a ghostview window that displays the CFG of the current
-- function.  Useful for debugging.
foreign import ccall unsafe "LLVMViewFunctionCFG"
  viewFunctionCFG :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMViewFunctionCFGOnly"
  viewFunctionCFGOnly :: ValueRef -> IO ()
