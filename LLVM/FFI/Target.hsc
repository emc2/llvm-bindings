{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
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

module LLVM.FFI.Target(
       TargetDataRef,
       TargetLibraryInfoRef,
       ByteOrdering,
       fromByteOrdering,
       toByteOrdering,
       createTargetData,
       addTargetData,
       addTargetLibraryInfo,
       copyStringRepOfTargetData,
       byteOrder,
       pointerSize,
       pointerType,
       intPtrType,
       sizeOfTypeInBits,
       storeSizeOfType,
       abiSizeOfType,
       abiAlignmentOfType,
       callFrameAlignmentOfType,
       preferredAlignmentOfType,
       preferredAlignmentOfGlobal,
       elementAtOffset,
       offsetOfElement,
       disposeTargetData
       ) where

import Data.Typeable
import Foreign.C.String (CString)

import Foreign.C.Types
import Foreign.Ptr (Ptr)

import LLVM.FFI.Core

#include <llvm-c/Target.h>

data ByteOrdering =
    BigEndian
  | LittleEndian

fromByteOrdering :: ByteOrdering -> CUInt
fromByteOrdering BigEndian = (#const LLVMBigEndian)
fromByteOrdering LittleEndian = (#const LLVMLittleEndian)

toByteOrdering :: CUInt -> ByteOrdering
toByteOrdering c
  | c == (#const LLVMBigEndian) = BigEndian
  | c == (#const LLVMLittleEndian) = LittleEndian

data TargetData
    deriving (Typeable)
type TargetDataRef = Ptr TargetData

data TargetLibraryInfo
    deriving (Typeable)

type TargetLibraryInfoRef = Ptr TargetLibraryInfo

-- | Creates target data from a target layout string.
foreign import ccall unsafe "LLVMCreateTargetData"
  createTargetData :: CString -> IO TargetDataRef

-- | Adds target data information to a pass manager. This does not
-- take ownership of the target data.
foreign import ccall unsafe "LLVMAddTargetData"
  addTargetData :: TargetDataRef -> PassManagerRef -> IO ()

-- | Adds target library information to a pass manager. This does not take
-- ownership of the target library info.
foreign import ccall unsafe "LLVMAddTargetLibraryInfo"
  addTargetLibraryInfo :: TargetLibraryInfoRef -> PassManagerRef -> IO ()

-- | Converts target data to a target layout string. The string must
-- be disposed with LLVMDisposeMessage.
foreign import ccall unsafe "LLVMCopyStringRepOfTargetData"
  copyStringRepOfTargetData :: TargetDataRef -> IO CString

-- | Returns the byte order of a target, either LLVMBigEndian or
-- LLVMLittleEndian.
foreign import ccall unsafe "LLVMByteOrder"
  byteOrder :: TargetDataRef -> CUInt

-- | Returns the pointer size in bytes for a target.
foreign import ccall unsafe "LLVMPointerSize"
  pointerSize :: TargetDataRef -> CUInt

-- | Returns the integer type that is the same size as a pointer on a target.
foreign import ccall unsafe "LLVMIntPtrType"
  intPtrType :: TargetDataRef -> TypeRef

-- | Computes the size of a type in bytes for a target.
foreign import ccall unsafe "LLVMSizeOfTypeInBits"
  sizeOfTypeInBits :: TargetDataRef -> TypeRef -> CULLong

-- | Computes the storage size of a type in bytes for a target.
foreign import ccall unsafe "LLVMStoreSizeOfType"
  storeSizeOfType :: TargetDataRef -> TypeRef -> CULLong

-- | Computes the ABI size of a type in bytes for a target.
foreign import ccall unsafe "LLVMABISizeOfType"
  abiSizeOfType :: TargetDataRef -> TypeRef -> CULLong

-- | Computes the ABI alignment of a type in bytes for a target.
foreign import ccall unsafe "LLVMABIAlignmentOfType"
  abiAlignmentOfType :: TargetDataRef -> TypeRef -> CULLong

-- | Computes the call frame alignment of a type in bytes for a target.
foreign import ccall unsafe "LLVMCallFrameAlignmentOfType"
  callFrameAlignmentOfType :: TargetDataRef -> TypeRef -> CULLong

-- | Computes the preferred alignment of a type in bytes for a target.
foreign import ccall unsafe "LLVMPreferredAlignmentOfType"
  preferredAlignmentOfType :: TargetDataRef -> TypeRef -> CULLong

-- | Computes the preferred alignment of a global variable in bytes
-- for a target.
foreign import ccall unsafe "LLVMPreferredAlignmentOfGlobal"
  preferredAlignmentOfGlobal :: TargetDataRef -> ValueRef -> CUInt

-- | Computes the structure element that contains the byte offset for a target.
foreign import ccall unsafe "LLVMElementAtOffset"
  elementAtOffset :: TargetDataRef -> TypeRef -> CULLong -> CUInt

-- | Computes the byte offset of the indexed struct element for a target.
foreign import ccall unsafe "LLVMOffsetOfElement"
  offsetOfElement :: TargetDataRef -> TypeRef -> CUInt -> CULLong

-- | Deallocates a TargetData.
foreign import ccall unsafe "LLVMDisposeTargetData"
  disposeTargetData :: TargetDataRef -> IO ()
