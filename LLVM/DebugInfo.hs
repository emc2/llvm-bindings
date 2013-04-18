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
{-# OPTIONS_GHC -Wall -Werror #-}

-- | This module contains common code for generating debugging information.
-- 
-- Note: the LLVM documentation of these structures is not very good.
-- This API may change slightly as I investigate further.
module LLVM.DebugInfo(
       llvmDebugVersion,
       compileUnitDesc,
       fileDesc,
       globalVarDesc,
       subprogramDesc,
       blockDesc,
       basicTypeDesc,
       derivedTypeDesc,
       compositeTypeDesc,
       enumDesc,
       localVarDesc,
       argDesc,
       locationDesc
       ) where

import Data.Dwarf
import Data.Word
import Data.Bits

import qualified LLVM.Core as LLVM

llvmDebugVersion :: Word
llvmDebugVersion = 0x80000

-- | Generate an LLVM metadata node containing debug info for a
-- compilation unit.
compileUnitDesc :: (Integral n1, Integral n2) => LLVM.ContextRef
                -- ^ The LLVM context.
                -> n1
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
                -> [LLVM.ValueRef]
                -- ^ The list of descriptors for enum types.
                -> [LLVM.ValueRef]
                -- ^ The list of descriptors for retained types.
                -> [LLVM.ValueRef]
                -- ^ The list of descriptors for subprograms.
                -> [LLVM.ValueRef]
                -- ^ The list of descriptors for global variables.
                -> IO LLVM.ValueRef
                -- ^ The LLVM Metadata for this file descriptor.
compileUnitDesc ctx lang file producer dir main opt flags
                vers enums types subprogs gvars =
  do
    int1ty <- LLVM.int1TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    filemd <- LLVM.mdStringInContext ctx file
    dirmd <- LLVM.mdStringInContext ctx dir
    producermd <- LLVM.mdStringInContext ctx producer
    flagsmd <- LLVM.mdStringInContext ctx flags
    enummd <- LLVM.mdNodeInContext ctx enums
    typemd <- LLVM.mdNodeInContext ctx types
    subprogmd <- LLVM.mdNodeInContext ctx subprogs
    gvarmd <- LLVM.mdNodeInContext ctx gvars
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty (dwTag_compile_unit + llvmDebugVersion) False,
        LLVM.constInt int32ty (0 :: Word) False,
        LLVM.constInt int32ty lang False,
        filemd, dirmd, producermd,
        LLVM.constInt int1ty (if main then 1 else 0 :: Word) False,
        LLVM.constInt int1ty (if opt then 1 else 0 :: Word) False,
        flagsmd,
        LLVM.constInt int32ty vers False,
        enummd, typemd, subprogmd, gvarmd ]

-- | Generate an LLVM metadata node containing a file name.
fileDesc :: LLVM.ContextRef
         -- ^ The LLVM context.
         -> String
         -- ^ The file name.
         -> String
         -- ^ The directory name.
         -> IO LLVM.ValueRef
         -- ^ The LLVM Metadata for this file descriptor.
fileDesc ctx file dir =
  do
    namemd <- LLVM.mdStringInContext ctx file
    dirmd <- LLVM.mdStringInContext ctx dir
    int32ty <- LLVM.int32TypeInContext ctx
    empty <- LLVM.mdNodeInContext ctx []
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty (dwTag_variable + llvmDebugVersion) False,
        namemd, dirmd, empty ]

-- | Generate an LLVM metadata node describing a global variable declaration.
globalVarDesc :: Integral n
              => LLVM.ContextRef
              -- ^ The LLVM context.
              -> LLVM.ValueRef
              -- ^ The metadata node for the compilation unit.
              -> String
              -- ^ The variable's name.
              -> String
              -- ^ The display name.
              -> String
              -- ^ The linkage name.
              -> LLVM.ValueRef
              -- ^ The metadata node for the file.
              -> n
              -- ^ The line number.
              -> LLVM.ValueRef
              -- ^ The metadata node for the type.
              -> Bool
              -- ^ Whether or not this subprogram is local to the
              -- compilation unit (static).
              -> Bool
              -- ^ Whether or not this subprogram is defined in the
              -- compilation unit (not extern).
              -> LLVM.ValueRef
              -- ^ A reference to the actual global variable itself.
              -> IO LLVM.ValueRef
              -- ^ The LLVM Metadata for this global variable.
globalVarDesc ctx compunitmd name dispname linkname filemd lineno
              typemd local defined ref =
  do
    int1ty <- LLVM.int1TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    namemd <- LLVM.mdStringInContext ctx name
    dispnamemd <- LLVM.mdStringInContext ctx dispname
    linknamemd <- LLVM.mdStringInContext ctx linkname
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty (dwTag_compile_unit + llvmDebugVersion) False,
        LLVM.constInt int32ty (0 :: Word) False,
        compunitmd, namemd, dispnamemd, linknamemd, filemd,
        LLVM.constInt int32ty lineno False,
        typemd,
        LLVM.constInt int1ty (if local then 1 else 0 :: Word) False,
        LLVM.constInt int1ty (if defined then 1 else 0 :: Word) False,
        ref ]

-- | Generate an LLVM metadata node containing information about a
-- subprogram (ie a function).
subprogramDesc :: (Integral n1, Integral n2, Integral n3)
               => LLVM.ContextRef
               -- ^ The LLVM context.
               -> LLVM.ValueRef
               -- ^ The metadata node for the compilation unit.
               -> String
               -- ^ The display name.
               -> String
               -- ^ The linkage name.
               -> LLVM.ValueRef
               -- ^ The metadata node for the file.
               -> n1
               -- ^ The line number.
               -> LLVM.ValueRef
               -- ^ The metadata node for the type.
               -> Bool
               -- ^ Whether or not this subprogram is local to the
               -- compilation unit (static).
               -> Bool
               -- ^ Whether or not this subprogram is defined in the
               -- compilation unit (not extern).
               -> LLVM.ValueRef
               -- ^ Indicates which base type contains the vtable
               -- pointer for the derived class (from LLVM debug info
               -- documentation).
               -> n2
               -- ^ Flags for the declaration.
               -> Bool
               -- ^ Whether or not this function is optimized.
               -> LLVM.ValueRef
               -- ^ A reference to the actual function itself.
               -> LLVM.ValueRef
               -- ^ A metadata containing the list of template parameters.
               -> LLVM.ValueRef
               -- ^ A metadata describing the function declaration.
               -> LLVM.ValueRef
               -- ^ A metadata containing the list of variables
               -- declared in the function.
               -> n3
               -- ^ The beginning line number.
               -> IO LLVM.ValueRef
               -- ^ The LLVM metadata for this function.
subprogramDesc ctx compunitmd dispname linkname filemd lineno typemd
               local defined basetype flags optimized ref
               tempparams funcdecl funcvars beginlineno =
  do
    int1ty <- LLVM.int1TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    dispnamemd <- LLVM.mdStringInContext ctx dispname
    linknamemd <- LLVM.mdStringInContext ctx linkname
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty (dwTag_subprogram + llvmDebugVersion) False,
        LLVM.constInt int32ty (0 :: Word) False,
        compunitmd, dispnamemd, linknamemd, filemd,
        LLVM.constInt int32ty lineno False,
        typemd,
        LLVM.constInt int1ty (if local then 1 else 0 :: Word) False,
        LLVM.constInt int1ty (if defined then 1 else 0 :: Word) False,
        basetype,
        LLVM.constInt int32ty flags False,
        LLVM.constInt int1ty (if optimized then 1 else 0 :: Word) False,
        ref, tempparams, funcdecl, funcvars,
        LLVM.constInt int32ty beginlineno False ]

-- | Generate an LLVM metadata node containing debug information for a
-- basic block.
blockDesc :: (Integral n1, Integral n2, Integral n3)
          => LLVM.ContextRef
          -- ^ The LLVM context.
          -> LLVM.ValueRef
          -- ^ The metadata for the subprogram containing this block.
          -> n1
          -- ^ The line number.
          -> n2
          -- ^ The column number.
          -> LLVM.ValueRef
          -- ^ The metadata for the file.
          -> n3
          -- ^ A unique identifier (for template instantiations).
          -> IO LLVM.ValueRef
          -- ^ The metadata describing the block.
blockDesc ctx subprogrammd lineno colno filemd uid =
  do
    int32ty <- LLVM.int32TypeInContext ctx
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty (dwTag_lexical_block + llvmDebugVersion) False,
        subprogrammd,
        LLVM.constInt int32ty lineno False,
        LLVM.constInt int32ty colno False,
        filemd,
        LLVM.constInt int32ty uid False ]

-- | Generate an llvm metadata node containing debug information for a
-- basic type.
basicTypeDesc :: (Integral n1, Integral n2, Integral n3,
                  Integral n4, Integral n5, Integral n6)
              => LLVM.ContextRef
              -- ^ The LLVM context.
              -> LLVM.ValueRef
              -- ^ The metadata for the compilation unit declaring
              -- this type.
              -> String
              -- ^ The type's name.
              -> LLVM.ValueRef
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
              -> IO LLVM.ValueRef
              -- ^ The LLVM metadata for this type.
basicTypeDesc ctx compunitmd name filemd lineno size
              align offset flags encoding =
  do
    int32ty <- LLVM.int32TypeInContext ctx
    int64ty <- LLVM.int64TypeInContext ctx
    namemd <- LLVM.mdStringInContext ctx name
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty (dwTag_base_type + llvmDebugVersion) False,
        compunitmd, namemd, filemd,
        LLVM.constInt int32ty lineno False,
        LLVM.constInt int64ty size False,
        LLVM.constInt int64ty align False,
        LLVM.constInt int64ty offset False,
        LLVM.constInt int32ty flags False,
        LLVM.constInt int32ty encoding False ]

-- | Generate an LLVM metadata node containing debug information for a
-- composite type.
derivedTypeDesc :: (Integral n1, Integral n2, Integral n3,
                    Integral n4, Integral n5, Integral n6)
                => LLVM.ContextRef
                -- ^ The LLVM context.
                -> n1
                -- ^ The tag.
                -> LLVM.ValueRef
                -- ^ The metadata for the compilation unit declaring
                -- this type.
                -> String
                -- ^ The type's name.
                -> LLVM.ValueRef
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
                -> LLVM.ValueRef
                -- ^ The metadata for the type from which this is derived
                -> IO LLVM.ValueRef
                -- ^ The metadata for this type.
derivedTypeDesc ctx tag compunitmd name filemd lineno
                size align offset flags derivemd =
  do
    int64ty <- LLVM.int64TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    namemd <- LLVM.mdStringInContext ctx name
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty ((fromIntegral tag) + llvmDebugVersion) False,
        compunitmd, compunitmd, namemd, filemd,
        LLVM.constInt int32ty lineno False,
        LLVM.constInt int64ty size False,
        LLVM.constInt int64ty align False,
        LLVM.constInt int64ty offset False,
        LLVM.constInt int32ty flags False,
        derivemd ]

-- | Generate an LLVM metadata node containing debug information for a
-- composite type.
compositeTypeDesc :: (Integral n1, Integral n2, Integral n3,
                      Integral n4, Integral n5, Integral n6)
                  => LLVM.ContextRef
                  -- ^ The LLVM context.
                  -> n1
                  -- ^ The tag
                  -> LLVM.ValueRef
                  -- ^ The metadata for the compilation unit declaring
                  -- this type.
                  -> String
                  -- ^ The type's name.
                  -> LLVM.ValueRef
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
                  -> LLVM.ValueRef
                  -- ^ The metadata for the type from which this is derived
                  -> LLVM.ValueRef
                  -- ^ Metadata array containing member types
                  -> IO LLVM.ValueRef
                  -- ^ The metadata for this type.
compositeTypeDesc ctx tag compunitmd name filemd lineno size
                  align offset flags derivemd membersmd =
  do
    int64ty <- LLVM.int64TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    namemd <- LLVM.mdStringInContext ctx name
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty ((fromIntegral tag) + llvmDebugVersion) False,
        compunitmd, compunitmd, namemd, filemd,
        LLVM.constInt int32ty lineno False,
        LLVM.constInt int64ty size False,
        LLVM.constInt int64ty align False,
        LLVM.constInt int64ty offset False,
        LLVM.constInt int32ty flags False,
        derivemd, membersmd ]

-- | Generate an LLVM metadata node containing debug information for
-- an enum value
enumDesc :: Integral n => LLVM.ContextRef
         -- ^ The LLVM context.
         -> String
         -- ^ The name.
         -> n
         -- ^ The enum value
         -> IO LLVM.ValueRef
         -- ^ The metadata for this enum.
enumDesc ctx name value =
  do
    int32ty <- LLVM.int32TypeInContext ctx
    namemd <- LLVM.mdStringInContext ctx name
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty (dwTag_enumerator + llvmDebugVersion) False,
        namemd,
        LLVM.constInt int32ty value False ]

-- | Generate an LLVM metadata node containing debug information for a
-- local variable.
localVarDesc :: (Integral n1, Integral n2)
             => LLVM.ContextRef
             -- ^ The LLVM context.
             -> LLVM.ValueRef
             -- ^ The metadata for the lexical block declaring this
             -- variable.
             -> String
             -- ^ The variable's name.
             -> LLVM.ValueRef
             -- ^ The metadata for the file declaring this variable.
             -> n1
             -- ^ Line number where defined.
             -> LLVM.ValueRef
             -- ^ The metadata for the type of this variable.
             -> n2
             -- ^ The flags.
             -> IO LLVM.ValueRef
             -- ^ The LLVM metadata for this variable.
localVarDesc ctx blockmd name filemd lineno typemd flags =
  do
    int32ty <- LLVM.int32TypeInContext ctx
    namemd <- LLVM.mdStringInContext ctx name
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty (256 + llvmDebugVersion) False,
        blockmd, namemd, filemd,
        LLVM.constInt int32ty lineno False,
        typemd,
        LLVM.constInt int32ty flags False ]

-- | Generate an LLVM metadata node containing debug information for
-- an argument
argDesc :: (Integral n1, Integral n2, Integral n3) => LLVM.ContextRef
        -- ^ The LLVM Context
        -> LLVM.ValueRef
        -- ^ The block metadata for the declarer of this variable
        -> String
        -- ^ The argument's name
        -> LLVM.ValueRef
        -- ^ The metadata for the file declaring this variable
        -> n1
        -- ^ Line number where defined
        -> n2
        -- ^ Argument index
        -> LLVM.ValueRef
        -- ^ The metadata for the type of this variable
        -> n3
        -- ^ The flags
        -> IO LLVM.ValueRef
        -- ^ The LLVM metadata for this variable
argDesc ctx blockmd name filemd lineno argno typemd flags =
  do
    int32ty <- LLVM.int32TypeInContext ctx
    namemd <- LLVM.mdStringInContext ctx name
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty (257 + llvmDebugVersion) False,
        blockmd, namemd, filemd,
        LLVM.constInt int32ty ((shiftL (fromIntegral argno) 24) .|.
                               (fromIntegral lineno) :: Word) False,
        typemd,
        LLVM.constInt int32ty flags False ]

-- | Generate an LLVM metadata node giving a location.
locationDesc :: (Integral n1, Integral n2) => LLVM.ContextRef
              -- ^ The LLVM Context.
              -> n1
              -- ^ The line number.
              -> n2
              -- ^ The column number.
              -> LLVM.ValueRef
              -- ^ The metadata node for the lexical block containing
              -- this.
              -> IO LLVM.ValueRef
locationDesc ctx lineno colno blockmd =
  do
    int32ty <- LLVM.int32TypeInContext ctx
    LLVM.mdNodeInContext ctx
      [ LLVM.constInt int32ty lineno False,
        LLVM.constInt int32ty colno False,
        blockmd ]
