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

-- | This module defines a class for monad which carry an LLVM module.
module Control.Monad.LLVM.LLVMModule.Class(
       MonadLLVMModule(..)
       ) where

import Control.Monad.Trans
import LLVM.Core(ModuleRef, ContextRef, ValueRef, TypeRef)

-- | Class for monads that carry LLVM context information
class MonadIO m => MonadLLVMModule m where
  -- | Obtain the data layout for a module.
  getDataLayout :: m String

  -- | Set the data layout for a module.  See Module::getDataLayout()
  setDataLayout :: String
                -- ^ Data layout string
                -> m ()

  -- | Obtain the target triple for a module.
  getTarget :: m String

  -- | Set the target triple for a module.
  setTarget :: String
            -- ^ Target triple string
            -> m ()

  -- | Dump a representation of a module to stderr.
  dumpModule :: m ()

  -- | Obtain a Type from a module by its registered name.
  getTypeByName :: String
                -- ^ Type name
                -> m TypeRef

  -- | Obtain the number of operands for named metadata in a module.
  getNamedMetadataNumOperands :: Integral n
                              => String
                              -- ^ Metadata name
                              -> m n
                              -- ^ Number of operands

  -- | Obtain the named metadata operands for a module.
  getNamedMetadataOperands :: String
                           -- ^ Metadata name
                           -> m [ValueRef]
                           -- ^ Operands

  -- | Add an operand to named metadata.
  addNamedMetadataOperand :: String
                          -- ^ Metadata name
                          -> ValueRef
                          -- ^ Operand (must be a metadata node)
                          -> m ()

  -- | Add a function to a module under a specified name.
  addFunction :: String
              -- ^ Function name
              -> TypeRef
              -- ^ Function type
              -> m ValueRef
              -- ^ Function value

  -- | Obtain a Function value from a Module by its name.
  -- 
  -- The returned value corresponds to a llvm::Function value.
  getNamedFunction :: String
                   -- ^ Function name
                   -> m ValueRef
                   -- ^ Function value (@nullPtr@ if not found)

  -- | Obtain an iterator to the first Function in a Module.
  getFirstFunction :: m ValueRef

  -- | Obtain an iterator to the last Function in a Module.
  getLastFunction :: m ValueRef

  -- | Obtain the context to which this module is associated.
  getModuleContext :: m ContextRef

  -- | Set inline assembly for a module.
  setModuleInlineAsm :: String
                     -- ^ Assembly string
                     -> m ()

  addGlobal :: TypeRef -> String -> m ValueRef

  addGlobalInAddressSpace :: Integral n
                          => ModuleRef
                          -- ^ Module
                          -> TypeRef
                          -- ^ Type
                          -> String
                          -- ^ Name
                          -> n
                          -- ^ Address space
                          -> m ValueRef
                          -- ^ Global variable

  getNamedGlobal :: String -> m ValueRef
  getFirstGlobal :: m ValueRef
  getLastGlobal :: m ValueRef

  addAlias :: TypeRef
           -- ^ Type
           -> ValueRef
           -- ^ Aliasee
           -> String
           -- ^ Name
           -> m ValueRef
           -- ^ Alias value