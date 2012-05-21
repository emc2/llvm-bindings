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

-- | Raw FFI bindings for llvm-c/BitWriter.h
module LLVM.FFI.BitWriter(
       writeBitcodeToFile,
       writeBitcodeToFD
       ) where

import Foreign.C.String(CString)
import Foreign.C.Types(CInt(..))
import Foreign.C.Types(CInt)

import LLVM.FFI.Core

-- | Writes a module to the specified path. Returns 0 on success.
foreign import ccall unsafe "LLVMWriteBitcodeToFile"
  writeBitcodeToFile :: ModuleRef
                     -- ^ Module
                     -> CString
                     -- ^ Filename
                     -> IO CInt
                     -- ^ Error code

-- | Writes a module to an open file descriptor. Returns 0 on success.
foreign import ccall unsafe "LLVMWriteBitcodeToFD"
  writeBitcodeToFD :: ModuleRef
                   -- ^ Module
                   -> CInt
                   -- ^ File descriptor
                   -> CInt
                   -- ^ Boolean, indicating whether the call should
                   -- close the descriptor
                   -> CInt
                   -- ^ Boolean, indicating whether the write should be
                   -- unbuffered.
                   -> IO CInt
                   -- ^ Error code
