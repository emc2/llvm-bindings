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
module LLVM.DebugInfo(
       llvmDebugVersion,
       compileUnitDesc,
       fileDesc,
       globalVarDesc,
       subprogramDesc,
       blockDesc,
       basicTypeDesc,
       derivedTypeDesc,
       combinedTypeDesc,
       localVarDesc,
       argDesc,
       locationDesc
       ) where

import Data.Dwarf

import qualified LLVM.Core as LLVM

llvmDebugVersion :: Word
llvmDebugVersion = 0x80000

-- | Generate an LLVM metadata node containing debug info for a
-- compilation unit.
compileUnitDesc :: Num (n1, n2) => LLVM.ContextRef
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
                -> IO LLVM.ValueRef
                -- ^ The LLVM Metadata for this file descriptor.
compileUnitDesc ctx lang file producer dir main opt flags vers =
  do
    int1ty <- LLVM.int1TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    tag <- LLVM.constInt int32ty (dwTag_compile_unit + llvmDebugVersion)
    tag <- LLVM.constInt int32ty lang
    zero <- LLVM.constInt int32ty 0
    filemd <- LLVM.mdStringInContext ctx file
    dirmd <- LLVM.mdStringInContext ctx dir
    producermd <- LLVM.mdStringInContext ctx producer
    mainmd <- LLVM.constInt int1ty main
    optmd <- LLVM.constInt int1ty opt
    flagsmd <- LLVM.mdStringInContext ctx flags
    optmd <- LLVM.constInt int32ty vers

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
    tag <- LLVM.constInt int32ty (dwTag_variable + llvmDebugVersion)
    empty <- LLVM.mdNodeInContext ctx []
    LLVM.mdNodeInContext ctx [ tag, namemd, dirmd, empty ]

-- | Generate an LLVM metadata node describing a global variable declaration.
globalVarDesc :: Num n => LLVM.ContextRef
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
              -> LLVM.ValueRef
              -- ^ The LLVM Metadata for this global variable.
globalVarDesc ctx compunitmd name dispname linkname filemd lineno
              typemd local defined ref =
  do
    int1ty <- LLVM.int1TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    zero <- LLVM.constInt int32ty 0
    tag <- LLVM.constInt int32ty (dwTag_compile_unit + llvmDebugVersion)
    namemd <- LLVM.mdStringInContext ctx name
    dispnamemd <- LLVM.mdStringInContext ctx dispname
    linknamemd <- LLVM.mdStringInContext ctx linkname
    linenomd <- LLVM.constInt int32ty lineno
    localmd <- LLVM.constInt int1ty local
    definedmd <- LLVM.constInt int1ty defined
    LLVM.mdNodeInContext ctx [ tag, zero, compunitmd, namemd, dispnamemd,
                               linknamemd, filemd, linenomd, typemd,
                               localmd, definedmd, ref ]

-- | Generate an LLVM metadata node containing information about a
-- subprogram (ie a function).
subprogramDesc :: Num (n1, n2, n3) => LLVM.ContextRef
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
               local defined basetype basetype flags optimized ref
               tempparams funcdecl funcvars beginlineno =
  do
    int1ty <- LLVM.int1TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    tag <- LLVM.constInt int32ty (dwTag_subprogram + llvmDebugVersion)
    dispnamemd <- LLVM.mdStringInContext ctx dispname
    linknamemd <- LLVM.mdStringInContext ctx linkname
    linenomd <- LLVM.constInt int32ty lineno
    localmd <- LLVM.constInt int1ty local
    definedmd <- LLVM.constInt int1ty defined
    flagsmd <- LLVM.constInt int32ty flags
    optimizedmd <- LLVM.constInt int1ty optimized
    beginlinenomd <- LLVM.constInt int32ty beginlineno
    LLVM.mdNodeInContext ctx [ tag, zero, compunitmd, dispname, linkname,
                               filemd, linenomd, typemd, localmd, definedmd,
                               basetype, flagsmd, optimizedmd, ref,
                               tempparams, funcdecl, funcvars, beginlinenomd ]

-- | Generate an LLVM metadata node containing debug information for a
-- basic block.
blockDesc :: LLVM.ContextRef
          -- ^ The LLVM context.
          -> IO LLVM.ValueRef
          -- ^ The metadata for the subprogram containing this block.
          -> Word
          -- ^ The line number.
          -> Word
          -- ^ The column number.
          -> IO LLVM.ValueRef
          -- ^ The metadata for the file.
          -> Word
          -- ^ A unique identifier (for template instantiations).
          -> IO LLVM.ValueRef
          -- ^ The metadata describing the block.
blockDesc ctx subprogrammd lineno colno filemd id =
  do
    int32ty <- LLVM.int32TypeInContext ctx
    tag <- LLVM.constInt int32ty (dwTag_lexical_block + llvmDebugVersion)
    linenomd <- LLVM.constInt int32ty lineno
    colnomd <- LLVM.constInt int32ty colno
    idmd <- LLVM.constInt int32ty id
    LLVM.mdNodeInContext ctx [ tag, subprogrammd, linenomd,
                               colnomd, filemd, idmd ]

-- | Generate an llvm metadata node containing debug information for a
-- basic type.
basicTypeDesc :: LLVM.ContextRef
              -- ^ The LLVM context.
              -> LLVM.ValueRef
              -- ^ The metadata for the compilation unit declaring
              -- this type.
              -> String
              -- ^ The type's name.
              -> LLVM.ValueRef
              -- ^ The metadata for the file.
              -> Word
              -- ^ The line number.
              -> Word
              -- ^ The size in bits.
              -> Word
              -- ^ The alignment in bits.
              -> Word
              -- ^ The offset in bits.
              -> Word
              -- ^ The flags.
              -> Word
              -- ^ The DWARF type encoding.
              -> IO LLVM.ValueRef
              -- ^ The LLVM metadata for this type.
basicTypeDesc ctx compunitmd name filemd lineno size
              align offset flags encoding=
  do
    int32ty <- LLVM.int32TypeInContext ctx
    int64ty <- LLVM.int64TypeInContext ctx
    tag <- LLVM.constInt int32ty (dwTag_base_type + llvmDebugVersion)
    namemd <- LLVM.mdStringInContext ctx file
    linenomd <- LLVM.constInt int32ty lineno
    sizemd <- LLVM.constInt int64ty size
    alignmd <- LLVM.constInt int64ty align
    offsetmd <- LLVM.constInt int64ty offset
    flagsmd <- LLVM.constInt int32ty flags
    encodingmd <- LLVM.constInt int32ty encoding
    LLVM.mdNodeInContext ctx [ tag, compunitmd, namemd, filemd, linenomd,
                               sizemd, alignmd, offsetmd, flagsmd, encodingmd ]

-- | Generate an LLVM metadata node containing debug information for a
-- composite type.
derivedTypeDesc :: LLVM.ContextRef
                -- ^ The LLVM context.
                -> LLVM.ValueRef
                -- ^ The metadata for the compilation unit declaring
                -- this type.
                -> String
                -- ^ The type's name.
                -> LLVM.ValueRef
                -- ^ The metadata for the file.
                -> Word
                -- ^ The line number.
                -> Word
                -- ^ The size in bits.
                -> Word
                -- ^ The alignment in bits.
                -> Word
                -- ^ The offset in bits.
                -> Word
                -- ^ The flags.
                -> LLVM.ValueRef
                -- ^ The metadata for the type from which this is derived
                -> IO LLVM.ValueRef
                -- ^ The metadata for this type.
derivedTypeDesc ctx tag compunitmd name filemd lineno
                size align offset flags, derivemd =
  do
    int64ty <- LLVM.int64TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    tag' <- LLVM.constInt int32ty (tag + llvmDebugVersion)
    namemd <- LLVM.mdStringInContext ctx file
    linenomd <- LLVM.constInt int32ty lineno
    sizemd <- LLVM.constInt int64ty size
    alignmd <- LLVM.constInt int64ty align
    offsetmd <- LLVM.constInt int64ty offset
    flagsmd <- LLVM.constInt int32ty flags
    LLVM.mdNodeInContext ctx [ tag', compunitmd, compunitmd, namemd,
                               filemd, linenomd, sizemd, alignmd,
                               offsetmd, flagsmd, derivemd ]

-- | Generate an LLVM metadata node containing debug information for a
-- composite type.
compositeTypeDesc :: Num (n1, n2, n3, n4, n5) => LLVM.ContextRef
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
                  -> LLVM.ValueRef
                  -- ^ The metadata for the type from which this is derived
                  -> LLVM.ValueRef
                  -- ^ Metadata array containing member types
                  -> IO LLVM.ValueRef
                  -- ^ The metadata for this type.
compositeTypeDesc ctx tag compunitmd name filemd lineno size
                  align offset flags, derivemd membersmd =
  do
    int64ty <- LLVM.int64TypeInContext ctx
    int32ty <- LLVM.int32TypeInContext ctx
    tag' <- LLVM.constInt int32ty (tag + llvmDebugVersion)
    namemd <- LLVM.mdStringInContext ctx file
    linenomd <- LLVM.constInt int32ty lineno
    sizemd <- LLVM.constInt int64ty size
    alignmd <- LLVM.constInt int64ty align
    offsetmd <- LLVM.constInt int64ty offset
    flagsmd <- LLVM.constInt int32ty flags
    LLVM.mdNodeInContext ctx [ tag', compunitmd, compunitmd, namemd,
                               filemd, linenomd, sizemd, alignmd,
                               offsetmd, flagsmd, derivemd, membersmd ]

-- | Generate an LLVM metadata node containing debug information for
-- an enum value
enumDesc :: Num n => LLVM.ContextRef
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
    tag <- LLVM.constInt int32ty (dwTag_enumerator + llvmDebugVersion)
    namemd <- LLVM.mdStringInContext ctx file
    LLVM.mdNodeInContext ctx [ tag, namemd, valuemd ]

-- | Generate an LLVM metadata node containing debug information for a
-- local variable.
localVarDesc :: Num (n1, n2) => LLVM.ContextRef
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
    tag <- LLVM.constInt int32ty (dwTag_auto_variable + llvmDebugVersion)
    namemd <- LLVM.mdStringInContext ctx file
    linenomd <- LLVM.constInt int32ty lineno
    flagsmd <- LLVM.constInt int32ty flags
    LLVM.mdNodeInContext ctx [ tag, blockmd, namemd, filemd,
                               linenomd, typemd flagsmd ]

-- | Generate an LLVM metadata node containing debug information for
-- an argument
argDesc :: Num (n1, n2, n3) => LLVM.ContextRef
        -- ^ The LLVM Context
        -> LLVM.ValueRef
        -- ^ The block metadata for the declarer of this variable
        -> String
        -- ^ The file name
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
    tag <- LLVM.constInt int32ty (dwTag_arg_variable + llvmDebugVersion)
    namemd <- LLVM.mdStringInContext ctx file
    linenomd <- LLVM.constInt int32ty ((shiftL argno 24) .|. lineno)
    flagsmd <- LLVM.constInt int32ty flags
    LLVM.mdNodeInContext ctx [ tag, blockmd, namemd, filemd,
                               linenomd, typemd flagsmd ]

-- | Generate an LLVM metadata node giving a location.
locationDesc :: Num (n1, n2) => LLVM.ContextRef
              -- ^ The LLVM Context.
              -> n1
              -- ^ The line number.
              -> n2
              -- ^ The column number.
              -> LLVM.ValueRef
              -- ^ The metadata node for the lexical block containing
              -- this.
              -> IO ()
locationDesc ctx val lineno colno blockmd =
  do
    int32ty <- LLVM.int32TypeInContext ctx
    linenomd <- LLVM.constInt int32ty lineno
    colnomd <- LLVM.constInt int32ty colno
    LLVM.mdNodeInContext ctx [ linenomd, colnomd, blockmd ]
