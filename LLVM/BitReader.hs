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

module LLVM.BitReader(
       parseBitcode,
       parseBitcodeInContext,
       getBitcodeModule,
       getBitcodeModuleInContext
       ) where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import LLVM.FFI.Core(ModuleRef, MemoryBufferRef, ContextRef)

import qualified LLVM.FFI.BitReader as FFI
import qualified LLVM.FFI.Core as FFI

-- | Builds a module from the bitcode in the specified memory buffer
parseBitcode :: MemoryBufferRef -> IO (Either ModuleRef String)
parseBitcode mbuf =
  let
    parseBitcode' pref sref =
      do
        res <- FFI.parseBitcode mbuf pref sref
        if res == 1
          then do
            out <- peek pref
            return (Left out)
          else do
            msg <- peek sref
            str <- peekCString msg
            FFI.disposeMessage msg
            return (Right str)
  in
    alloca (\ref -> alloca (parseBitcode' ref))

parseBitcodeInContext :: ContextRef -> MemoryBufferRef ->
                         IO (Either ModuleRef String)
parseBitcodeInContext ctx mbuf =
  let
    parseBitcodeInContext' pref sref =
      do
        res <- FFI.parseBitcodeInContext ctx mbuf pref sref
        if res == 1
          then do
            out <- peek pref
            return (Left out)
          else do
            msg <- peek sref
            str <- peekCString msg
            FFI.disposeMessage msg
            return (Right str)
  in
    alloca (\ref -> alloca (parseBitcodeInContext' ref))

-- | Reads a module from the specified path,
getBitcodeModule :: MemoryBufferRef -> IO (Either ModuleRef String)
getBitcodeModule mod =
  let
    getBitcodeModule' pref sref =
      do
        res <- FFI.getBitcodeModule mod pref sref
        if res == 1
          then do
            out <- peek pref
            return (Left out)
          else do
            msg <- peek sref
            str <- peekCString msg
            FFI.disposeMessage msg
            return (Right str)
  in
    alloca (\ref -> alloca (getBitcodeModule' ref))

getBitcodeModuleInContext :: ContextRef -> MemoryBufferRef ->
                             IO (Either ModuleRef String)
getBitcodeModuleInContext ctx mbuf =
  let
    getBitcodeModuleInContext' pref sref =
      do
        res <- FFI.getBitcodeModuleInContext ctx mbuf pref sref
        if res == 1
          then do
            out <- peek pref
            return (Left out)
          else do
            msg <- peek sref
            str <- peekCString msg
            FFI.disposeMessage msg
            return (Right str)
  in
    alloca (\ref -> alloca (getBitcodeModuleInContext' ref))
