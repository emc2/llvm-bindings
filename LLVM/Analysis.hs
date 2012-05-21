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

-- | Bindings for llvm-c/Analysis.h, wrapped in utility code to make them
-- more usable for general purposes.
module LLVM.Analysis(
       verifyModule,
       verifyFunction,
       viewFunctionCFG,
       viewFunctionCFGOnly
       ) where

import Foreign.C.String
import Foreign.C.Types(CInt(..))
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr(Ptr)
import Foreign.Storable
import LLVM.FFI.Core(ModuleRef, ValueRef)

import qualified LLVM.FFI.Analysis as FFI
import qualified LLVM.FFI.Core as FFI

action = FFI.fromVerifierFailureAction FFI.ReturnStatus

-- | Verifies that a module is valid, taking the specified action if
-- not.  Optionally returns a human-readable description of any
-- invalid constructs.  Message must be disposed with
-- @disposeMessage@.
verifyModule :: ModuleRef -> IO (Bool, String)
verifyModule mod =
  let
    verifyModule' ptr =
      do
        res <- FFI.verifyModule mod action ptr
        msg <- peek ptr
        str <- peekCString msg
        FFI.disposeMessage msg
        return (not (toBool res), str)
  in
    alloca verifyModule'
     
-- | Verifies that a single function is valid, taking the specified
-- action. Useful for debugging.
verifyFunction :: ValueRef -> IO Bool
verifyFunction func =
  do
    res <- FFI.verifyFunction func action
    return (res == 1)

-- | Open up a ghostview window that displays the CFG of the current
-- function.  Useful for debugging.
viewFunctionCFG :: ValueRef -> IO ()
viewFunctionCFG = FFI.viewFunctionCFG

viewFunctionCFGOnly :: ValueRef -> IO ()
viewFunctionCFGOnly = FFI.viewFunctionCFGOnly