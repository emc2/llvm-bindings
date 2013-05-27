-- Copyright (c) 2013 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | This module defines a monad which carries an LLVM context.
module Control.Monad.LLVM.LLVMContext.Class(
       MonadLLVMContext(..)
       ) where

import Control.Monad.Trans
import LLVM.Core(ModuleRef, ValueRef, TypeRef,
                 BasicBlockRef, BuilderRef)

-- | Class for monads that carry LLVM context information
class MonadIO m => MonadLLVMContext m where
  -- | Get the kind ID for a metadata tag.  This is used to attach
  -- specific metadata to an instruction.  For example, look up "dbg" to
  -- get the kind ID used to attach !dbg metadata.
  getMDKindIDInContext :: Num n
                       => String
                       -- ^ Metadata name
                       -> m n

  -- | Create a new, empty module in a specific context.
  -- 
  -- Every invocation should be paired with disposeModule() or memory
  -- will be leaked.
  moduleCreateWithNameInContext :: String
                                -- ^ Module name
                                -> m ModuleRef

  int1TypeInContext :: m TypeRef
  int8TypeInContext :: m TypeRef
  int16TypeInContext :: m TypeRef
  int32TypeInContext :: m TypeRef
  int64TypeInContext :: m TypeRef
  intTypeInContext :: Integral n => n -> m TypeRef
  floatTypeInContext :: m TypeRef
  doubleTypeInContext :: m TypeRef
  x86FP80TypeInContext :: m TypeRef
  fp128TypeInContext :: m TypeRef
  ppcFP128TypeInContext :: m TypeRef

  -- | Create a new structure type in a context.
  -- 
  -- A structure is specified by a list of inner elements/types and
  -- whether these can be packed together.
  structTypeInContext :: [TypeRef]
                      -- ^ Element types
                      -> Bool
                      -- ^ True if the structure is packed
                      -> m TypeRef
                      -- ^ Structure type

  -- | Create an empty structure in a context having a specified name.
  structCreateNamed :: String
                    -- ^ Structure name
                    -> m TypeRef
                    -- ^ Structure type

  voidTypeInContext :: m TypeRef
  labelTypeInContext :: m TypeRef
  x86MMXTypeInContext :: m TypeRef

  -- | Create a ConstantDataSequential and initialize it with a string.
  constStringInContext :: String
                       -- ^ String constant
                       -> Bool
                       -- ^ Whether the string is null-terminated.
                       -> m ValueRef
                       -- ^ String constant

  -- | Create an anonymous ConstantStruct with the specified values.
  constStructInContext :: [ValueRef]
                       -- ^ Elements
                       -> Bool
                       -- ^ Whether the structure is packed
                       -> m ValueRef
                       -- ^ Constant value

  -- | Obtain a MDString value from a context.
  -- 
  -- The returned instance corresponds to the llvm::MDString class.
  -- 
  -- The instance is specified by string data of a specified length. The
  -- string content is copied, so the backing memory can be freed after
  -- this function returns.
  mdStringInContext :: String
                    -- ^ Metadata string
                    -> m ValueRef
                    -- ^ Metadata node value

  -- | Obtain a MDNode value from a context.
  -- 
  -- The returned value corresponds to the llvm::MDNode class.
  mdNodeInContext :: [ValueRef]
                  -- ^ Values
                  -> m ValueRef
                  -- ^ Metadata node value

  -- | Append a basic block to the end of a function.
  appendBasicBlockInContext :: ValueRef
                            -- ^ The function to which to append the block.
                            -> String
                            -- ^ The name of the block.
                            -> m BasicBlockRef
                            -- ^ The block.

  -- | Insert a basic block in a function before another basic block.
  -- 
  -- The function to add to is determined by the function of the
  -- passed basic block.
  insertBasicBlockInContext :: BasicBlockRef
                            -- ^ The function into which to insert the block.
                            -> String
                            -- ^ The name of the block.
                            -> m BasicBlockRef
                            -- ^ The block.

  createBuilderInContext :: m BuilderRef
