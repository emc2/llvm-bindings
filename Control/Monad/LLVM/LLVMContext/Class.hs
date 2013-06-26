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
import LLVM.Core(ModuleRef, ValueRef, TypeRef, BasicBlockRef,
                 BuilderRef, MemoryBufferRef)

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

  parseBitcodeInContext :: MemoryBufferRef -> m (Either ModuleRef String)
  getBitcodeModuleInContext :: MemoryBufferRef -> m (Either ModuleRef String)

  -- | Create metadata for a TBAA root node in a context.
  tbaaRootMetadataInContext :: String
                            -- ^ The name to give the root metadata.
                            -> m ValueRef
                            -- ^ The TBAA metadata node.

  -- | Create metadata for a TBAA branch node in a context.
  tbaaMetadataInContext :: String
                        -- ^ The name of the type.
                        -> ValueRef
                        -- ^ The parent TBAA node.
                        -> Bool
                        -- ^ Whether or not the TBAA node represents a
                        -- constant type.
                        -> m ValueRef
                        -- ^ The TBAA metadata node.

  -- | Create an LLVM range metadata value in a given context for an
  -- integer type with a given width from a list of intervals.
  -- 
  -- Intervals are represented as a list of (a, b) pairs, where a
  -- represents the inclusive lower bound and b represents the exclusive
  -- upper bound.  That is, (a, b) represents the mathematical interval
  -- [a, b).  It must always be the case that a < b.
  -- 
  -- The intervals in a list may be overlapping, unsorted, and may
  -- contain values anywhere in the range -2^w to 2^w, where w is the
  -- width of the integer type.  The list will be automatically
  -- converted to the format expected by LLVM.
  rangeMetadataInContext :: (Integral n1, Integral n2, Ord n2)
                         => n1
                         -- ^ The width of the integer type in bits.
                         -> [(n2, n2)]
                         -- ^ The intervals (unsorted, possibly
                         -- overlapping, not wrapped)
                         -> m (Maybe ValueRef)
                         -- ^ A metadata value, on Nothing if the range covers
                         -- the entire space of values.


  -- | Create metadata for FP math accuracy in a given context.
  fpMathMetadataInContext :: Real n => n
                          -- ^ The required accuracy.
                          -> m ValueRef
                          -- ^ The FP math metadata node.

  -- | Create a loop identifier metadata node in the given context.
  loopMetadataInContext :: m ValueRef

  -- | Generate an LLVM metadata node containing debug info for a
  -- compilation unit.
  compileUnitMetadataInContext :: (Integral n1, Integral n2) => n1
                               -- ^ Language ID.
                               -> String
                               -- ^ The file name.
                               -> String
                               -- ^ The directory name.
                               -> String
                               -- ^ The producer name.
                               -> Bool
                               -- ^ Whether or not this is a main compile unit.
                               -> Bool
                               -- ^ Whether or not this is optimized.
                               -> String
                               -- ^ The flags for this compile unit.
                               -> n2
                               -- ^ The runtime version.
                               -> [ValueRef]
                               -- ^ The list of descriptors for enum types.
                               -> [ValueRef]
                               -- ^ The list of descriptors for retained types.
                               -> [ValueRef]
                               -- ^ The list of descriptors for subprograms.
                               -> [ValueRef]
                               -- ^ The list of descriptors for global
                               -- variables.
                               -> m ValueRef
                               -- ^ The LLVM Metadata for this file descriptor.

  -- | Generate an LLVM metadata node containing a file name.
  fileMetadataInContext :: String
                        -- ^ The file name.
                        -> String
                        -- ^ The directory name.
                        -> m ValueRef
                        -- ^ The LLVM Metadata for this file descriptor.

  -- | Generate an LLVM metadata node describing a global variable declaration.
  globalVarMetadataInContext :: Integral n => ValueRef
                             -- ^ The metadata node for the compilation unit.
                             -> String
                             -- ^ The variable's name.
                             -> String
                             -- ^ The display name.
                             -> String
                             -- ^ The linkage name.
                             -> ValueRef
                             -- ^ The metadata node for the file.
                             -> n
                             -- ^ The line number.
                             -> ValueRef
                             -- ^ The metadata node for the type.
                             -> Bool
                             -- ^ Whether or not this subprogram is
                             -- local to the compilation unit
                             -- (static).
                             -> Bool
                             -- ^ Whether or not this subprogram is
                             -- defined in the compilation unit (not
                             -- extern).
                             -> ValueRef
                             -- ^ A reference to the actual global
                             -- variable itself.
                             -> m ValueRef
                             -- ^ The LLVM Metadata for this global variable.

  -- | Generate an LLVM metadata node containing information about a
  -- subprogram (ie a function).
  subprogramMetadataInContext :: (Integral n1, Integral n2, Integral n3)
                              => ValueRef
                              -- ^ The metadata node for the compilation unit.
                              -> String
                              -- ^ The display name.
                              -> String
                              -- ^ The linkage name.
                              -> ValueRef
                              -- ^ The metadata node for the file.
                              -> n1
                              -- ^ The line number.
                              -> ValueRef
                              -- ^ The metadata node for the type.
                              -> Bool
                              -- ^ Whether or not this subprogram is
                              -- local to the compilation unit (static).
                              -> Bool
                              -- ^ Whether or not this subprogram is
                              -- defined in the compilation unit (not
                              -- extern).
                              -> ValueRef
                              -- ^ Indicates which base type contains
                              -- the vtable pointer for the derived
                              -- class (from LLVM debug info
                              -- documentation).
                              -> n2
                              -- ^ Flags for the declaration.
                              -> Bool
                              -- ^ Whether or not this function is optimized.
                              -> ValueRef
                              -- ^ A reference to the actual function itself.
                              -> ValueRef
                              -- ^ A metadata containing the list of
                              -- template parameters.
                              -> ValueRef
                              -- ^ A metadata describing the function
                              -- declaration.
                              -> ValueRef
                              -- ^ A metadata containing the list of variables
                              -- declared in the function.
                              -> n3
                              -- ^ The beginning line number.
                              -> m ValueRef
                              -- ^ The LLVM metadata for this function.

  -- | Generate an LLVM metadata node containing debug information for a
  -- basic block.
  blockMetadataInContext :: (Integral n1, Integral n2, Integral n3)
                         => ValueRef
                         -- ^ The metadata for the subprogram containing
                         -- this block.
                         -> n1
                         -- ^ The line number.
                         -> n2
                         -- ^ The column number.
                         -> ValueRef
                         -- ^ The metadata for the file.
                         -> n3
                         -- ^ A unique identifier (for template instantiations).
                         -> m ValueRef
                         -- ^ The metadata describing the block.

  -- | Generate an llvm metadata node containing debug information for a
  -- basic type.
  basicTypeMetadataInContext :: (Integral n1, Integral n2, Integral n3,
                                 Integral n4, Integral n5, Integral n6)
                             => ValueRef
                             -- ^ The metadata for the compilation
                             -- unit declaring this type.
                             -> String
                             -- ^ The type's name.
                             -> ValueRef
                             -- ^ The metadata for the file.
                             -> n1
                             -- ^ The line number.
                             -> n2
                             -- ^ The size in bits.
                             -> n3
                             -- ^ The alignment in bits.
                             -> n4
                             -- ^ The offset in bits.
                             -> n5
                             -- ^ The flags.
                             -> n6
                             -- ^ The DWARF type encoding.
                             -> m ValueRef
                             -- ^ The LLVM metadata for this type.

  -- | Generate an LLVM metadata node containing debug information for a
  -- composite type.
  derivedTypeMetadataInContext :: (Integral n1, Integral n2, Integral n3,
                                   Integral n4, Integral n5, Integral n6)
                               => n1
                               -- ^ The tag.
                               -> ValueRef
                               -- ^ The metadata for the compilation
                               -- unit declaring this type.
                               -> String
                               -- ^ The type's name.
                               -> ValueRef
                               -- ^ The metadata for the file.
                               -> n2
                               -- ^ The line number.
                               -> n3
                               -- ^ The size in bits.
                               -> n4
                               -- ^ The alignment in bits.
                               -> n5
                               -- ^ The offset in bits.
                               -> n6
                               -- ^ The flags.
                               -> ValueRef
                               -- ^ The metadata for the type from
                               -- which this is derived
                               -> m ValueRef
                               -- ^ The metadata for this type.

  -- | Generate an LLVM metadata node containing debug information for a
  -- composite type.
  compositeTypeMetadataInContext :: (Integral n1, Integral n2, Integral n3,
                                     Integral n4, Integral n5, Integral n6)
                                 => n1
                                 -- ^ The tag
                                 -> ValueRef
                                 -- ^ The metadata for the compilation
                                 -- unit declaring this type.
                                 -> String
                                 -- ^ The type's name.
                                 -> ValueRef
                                 -- ^ The metadata for the file.
                                 -> n2
                                 -- ^ The line number.
                                 -> n3
                                 -- ^ The size in bits.
                                 -> n4
                                 -- ^ The alignment in bits.
                                 -> n5
                                 -- ^ The offset in bits.
                                 -> n6
                                 -- ^ The flags.
                                 -> ValueRef
                                 -- ^ The metadata for the type from
                                 -- which this is derived
                                 -> ValueRef
                                 -- ^ Metadata array containing member types
                                 -> m ValueRef
                                 -- ^ The metadata for this type.

  -- | Generate an LLVM metadata node containing debug information for
  -- an enum value
  enumMetadataInContext :: Integral n => String
                        -- ^ The name.
                        -> n
                        -- ^ The enum value
                        -> m ValueRef
                        -- ^ The metadata for this enum.

  -- | Generate an LLVM metadata node containing debug information for a
  -- local variable.
  localVarMetadataInContext :: (Integral n1, Integral n2) => ValueRef
                            -- ^ The metadata for the lexical block
                            -- declaring this variable.
                            -> String
                            -- ^ The variable's name.
                            -> ValueRef
                            -- ^ The metadata for the file declaring
                            -- this variable.
                            -> n1
                            -- ^ Line number where defined.
                            -> ValueRef
                            -- ^ The metadata for the type of this variable.
                            -> n2
                            -- ^ The flags.
                            -> m ValueRef
                            -- ^ The LLVM metadata for this variable.

  -- | Generate an LLVM metadata node containing debug information for
  -- an argument
  argMetadataInContext :: (Integral n1, Integral n2, Integral n3) => ValueRef
                       -- ^ The block metadata for the declarer of this variable
                       -> String
                       -- ^ The argument's name
                       -> ValueRef
                       -- ^ The metadata for the file declaring this variable
                       -> n1
                       -- ^ Line number where defined
                       -> n2
                       -- ^ Argument index
                       -> ValueRef
                       -- ^ The metadata for the type of this variable
                       -> n3
                       -- ^ The flags
                       -> m ValueRef
                       -- ^ The LLVM metadata for this variable

  -- | Generate an LLVM metadata node giving a location.
  locationMetadataInContext :: (Integral n1, Integral n2) => n1
                            -- ^ The line number.
                            -> n2
                            -- ^ The column number.
                            -> ValueRef
                            -- ^ The metadata node for the lexical block
                            -- containing this.
                            -> m ValueRef
