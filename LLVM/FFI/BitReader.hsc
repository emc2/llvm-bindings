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

-- | Raw FFI bindings for llvm-c/BitReader.h
module LLVM.FFI.BitReader(
       getBitcodeModuleProvider,
       getBitcodeModuleProviderInContext,
       parseBitcode,
       parseBitcodeInContext,
       getBitcodeModule,
       getBitcodeModuleInContext
       ) where

import Foreign.C.String(CString)
import Foreign.C.Types(CInt(..))
import Foreign.Ptr(Ptr)

import LLVM.FFI.Core

-- | Builds a module from the bitcode in the specified memory buffer
foreign import ccall unsafe "LLVMParseBitcode"
  parseBitcode :: MemoryBufferRef
               -- ^ Memory buffer containing bitcode
               -> Ptr ModuleRef
               -- ^ Module reference stored here if successful
               -> Ptr CString
               -- ^ Error string stored here if not null
               -> IO CInt
               -- ^ Error code, 0 if successful

foreign import ccall unsafe "LLVMParseBitcodeInContext"
  parseBitcodeInContext :: ContextRef
                        -> MemoryBufferRef
                        -- ^ Memory buffer containing bitcode
                        -> Ptr ModuleRef
                        -- ^ Module reference stored here if successful
                        -> Ptr CString
                        -- ^ Error string stored here if not null
                        -> IO CInt
                        -- ^ Error code, 0 if successful

-- | Reads a module from the specified path,
foreign import ccall unsafe "LLVMGetBitcodeModule"
  getBitcodeModule :: MemoryBufferRef
                   -- ^ Memory buffer containing bitcode
                   -> Ptr ModuleRef
                   -- ^ Module reference stored here if successful
                   -> Ptr CString
                   -- ^ Error string stored here if not null
                   -> IO CInt
                   -- ^ Error code, 0 if successful

foreign import ccall unsafe "LLVMGetBitcodeModuleInContext"
  getBitcodeModuleInContext :: ContextRef
                            -- ^ Context
                            -> MemoryBufferRef
                            -- ^ Memory buffer containing bitcode
                            -> Ptr ModuleRef
                            -- ^ Module reference stored here if successful
                            -> Ptr CString
                            -- ^ Error string stored here if not null
                            -> IO CInt
                            -- ^ Error code, 0 if successful

-- | Deprecated: Use LLVMGetBitcodeModuleInContext instead.
foreign import ccall unsafe "LLVMGetBitcodeModuleProvider"
  getBitcodeModuleProvider :: MemoryBufferRef
                           -- ^ Memory buffer containing bitcode
                           -> Ptr ModuleProviderRef
                           -- ^ Module reference stored here if successful
                           -> Ptr CString
                           -- ^ Error string stored here if not null
                           -> IO CInt
                           -- ^ Error code, 0 if successful

-- | Deprecated: Use LLVMGetBitcodeModule instead.
foreign import ccall unsafe "LLVMGetBitcodeModuleProviderInContext"
  getBitcodeModuleProviderInContext :: ContextRef
                                    -- ^ Context
                                    -> MemoryBufferRef
                                    -- ^ Memory buffer containing bitcode
                                    -> Ptr ModuleRef
                                    -- ^ Module reference stored here
                                    -- if successful
                                    -> Ptr CString
                                    -- ^ Error string stored here if not null
                                    -> IO CInt
                                    -- ^ Error code, 0 if successful
