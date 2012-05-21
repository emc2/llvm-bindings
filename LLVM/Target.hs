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

module LLVM.Target(
       TargetDataRef,
       TargetLibraryInfoRef,
       ByteOrdering,
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

import Data.Word
import Foreign.C.String
import LLVM.FFI.Core
import LLVM.FFI.Target(TargetDataRef, TargetLibraryInfoRef)

import qualified LLVM.FFI.Target as FFI

type ByteOrdering = FFI.ByteOrdering

-- | Creates target data from a target layout string.
createTargetData :: String -> IO TargetDataRef
createTargetData str = withCString str FFI.createTargetData

-- | Adds target data information to a pass manager. This does not
-- take ownership of the target data.
addTargetData :: TargetDataRef -> PassManagerRef -> IO ()
addTargetData = FFI.addTargetData

-- | Adds target library information to a pass manager. This does not take
-- ownership of the target library info.
addTargetLibraryInfo :: TargetLibraryInfoRef -> PassManagerRef -> IO ()
addTargetLibraryInfo = FFI.addTargetLibraryInfo

-- | Converts target data to a target layout string. The string must
-- be disposed with LLVMDisposeMessage.
copyStringRepOfTargetData :: TargetDataRef -> IO String
copyStringRepOfTargetData t = FFI.copyStringRepOfTargetData t >>= peekCString

-- | Returns the byte order of a target, either LLVMBigEndian or
-- LLVMLittleEndian.
byteOrder :: TargetDataRef -> ByteOrdering
byteOrder = FFI.toByteOrdering . FFI.byteOrder

-- | Returns the pointer size in bytes for a target.
pointerSize :: Num n => TargetDataRef -> n
pointerSize = fromIntegral . FFI.pointerSize

-- | Returns the integer type that is the same size as a pointer on a target.
intPtrType :: TargetDataRef -> TypeRef
intPtrType = FFI.intPtrType

-- | Computes the size of a type in bytes for a target.
sizeOfTypeInBits :: Num n => TargetDataRef -> TypeRef -> n
sizeOfTypeInBits targ = fromIntegral . FFI.sizeOfTypeInBits targ

-- | Computes the storage size of a type in bytes for a target.
storeSizeOfType :: Num n => TargetDataRef -> TypeRef -> n
storeSizeOfType targ = fromIntegral . FFI.storeSizeOfType targ

-- | Computes the ABI size of a type in bytes for a target.
abiSizeOfType :: Num n => TargetDataRef -> TypeRef -> n
abiSizeOfType targ = fromIntegral . FFI.abiSizeOfType targ

-- | Computes the ABI alignment of a type in bytes for a target.
abiAlignmentOfType :: Num n => TargetDataRef -> TypeRef -> n
abiAlignmentOfType targ = fromIntegral . FFI.abiAlignmentOfType targ

-- | Computes the call frame alignment of a type in bytes for a target.
callFrameAlignmentOfType :: Num n => TargetDataRef -> TypeRef -> n
callFrameAlignmentOfType targ =
  fromIntegral . FFI.callFrameAlignmentOfType targ

-- | Computes the preferred alignment of a type in bytes for a target.
preferredAlignmentOfType :: Num n => TargetDataRef -> TypeRef -> n
preferredAlignmentOfType targ =
  fromIntegral . FFI.preferredAlignmentOfType targ

-- | Computes the preferred alignment of a global variable in bytes
-- for a target.
preferredAlignmentOfGlobal :: Num n => TargetDataRef -> ValueRef -> n
preferredAlignmentOfGlobal targ =
  fromIntegral . FFI.preferredAlignmentOfGlobal targ

-- | Computes the structure element that contains the byte offset for a target.
elementAtOffset :: (Integral m, Num n) => TargetDataRef -> TypeRef -> m -> n
elementAtOffset targ ty =
  fromIntegral . FFI.elementAtOffset targ ty . fromIntegral

-- | Computes the byte offset of the indexed struct element for a target.
offsetOfElement :: (Integral m, Num n) => TargetDataRef -> TypeRef -> m -> n
offsetOfElement targ ty =
  fromIntegral . FFI.offsetOfElement targ ty . fromIntegral

-- | Deallocates a TargetData.
disposeTargetData = FFI.disposeTargetData