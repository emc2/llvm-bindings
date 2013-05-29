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

-- | Utility functions for creating metadata.
module LLVM.Metadata(
       -- * TBAA Metadata
       tbaaRootMetadata,
       tbaaMetadata,
       tbaaRootMetadataInContext,
       tbaaMetadataInContext,

       -- * FP Math Metadata
       fpMathMetadata,
       fpMathMetadataInContext,

       -- * Range Metadata
--       rangeMetadata,
--       rangeMetadataInContext,

       -- * Loop Metadata
       loopMetadata,
       loopMetadataInContext,

       -- * Debug metadata
       llvmDebugVersion,
       compileUnitMetadata,
       fileMetadata,
       globalVarMetadata,
       subprogramMetadata,
       blockMetadata,
       basicTypeMetadata,
       derivedTypeMetadata,
       compositeTypeMetadata,
       enumMetadata,
       localVarMetadata,
       argMetadata,
       locationMetadata,
       compileUnitMetadataInContext,
       fileMetadataInContext,
       globalVarMetadataInContext,
       subprogramMetadataInContext,
       blockMetadataInContext,
       basicTypeMetadataInContext,
       derivedTypeMetadataInContext,
       compositeTypeMetadataInContext,
       enumMetadataInContext,
       localVarMetadataInContext,
       argMetadataInContext,
       locationMetadataInContext,
       ) where

import Data.Dwarf
import Data.Word
import Data.Bits
import LLVM.Core

-- | Create metadata for a TBAA root node.
tbaaRootMetadata :: String
                 -- ^ The name to give the root metadata.
                 -> IO ValueRef
                 -- ^ The TBAA metadata node.
tbaaRootMetadata name =
  do
    str <- mdString name
    mdNode [str]

-- | Create metadata for a TBAA branch node.
tbaaMetadata :: String
             -- ^ The name of the type.
             -> ValueRef
             -- ^ The parent TBAA node.
             -> Bool
             -- ^ Whether or not the TBAA node represents a constant type.
             -> IO ValueRef
             -- ^ The TBAA metadata node.
tbaaMetadata name parent False =
  do
    str <- mdString name
    mdNode [str, parent]
tbaaMetadata name parent True =
  do
    str <- mdString name
    mdNode [str, parent, constInt int1Type 1 False]

-- | Create metadata for a TBAA root node in a context.
tbaaRootMetadataInContext :: ContextRef
                          -- ^ The LLVM Context.
                          -> String
                          -- ^ The name to give the root metadata.
                          -> IO ValueRef
                          -- ^ The TBAA metadata node.
tbaaRootMetadataInContext ctx name =
  do
    str <- mdStringInContext ctx name
    mdNodeInContext ctx [str]

-- | Create metadata for a TBAA branch node in a context.
tbaaMetadataInContext :: ContextRef
                      -- ^ The LLVM Context.
                      -> String
                      -- ^ The name of the type.
                      -> ValueRef
                      -- ^ The parent TBAA node.
                      -> Bool
                      -- ^ Whether or not the TBAA node represents a
                      -- constant type.
                      -> IO ValueRef
                      -- ^ The TBAA metadata node.
tbaaMetadataInContext ctx name parent False =
  do
    str <- mdStringInContext ctx name
    mdNodeInContext ctx [str, parent]
tbaaMetadataInContext ctx name parent True =
  do
    str <- mdStringInContext ctx name
    ty <- int1TypeInContext ctx
    mdNodeInContext ctx [str, parent, constInt ty 1 False]

-- | Create metadata for FP math accuracy.
fpMathMetadata :: Real n => n
               -- ^ The required accuracy.
               -> IO ValueRef
               -- ^ The FP math metadata node.
fpMathMetadata accuracy = mdNode [constReal doubleType accuracy]

-- | Create metadata for FP math accuracy in a given context.
fpMathMetadataInContext :: Real n => ContextRef
                        -- ^ The LLVM Context.
                        -> n
                        -- ^ The required accuracy.
                        -> IO ValueRef
                        -- ^ The FP math metadata node.
fpMathMetadataInContext ctx accuracy =
  do
    ty <- doubleTypeInContext ctx
    mdNode [constReal ty accuracy]

-- | Create a loop identifier metadata node.
loopMetadata :: IO ValueRef
loopMetadata =
  do
    node <- mdNode [ constPointerNull voidType ]
    setOperand node 0 node
    return node

-- | Create a loop identifier metadata node in the given context.
loopMetadataInContext :: ContextRef
                      -- ^ The LLVM Context.
                      -> IO ValueRef
loopMetadataInContext ctx =
  do
    node <- mdNodeInContext ctx [ constPointerNull voidType ]
    setOperand node 0 node
    return node

llvmDebugVersion :: Word
llvmDebugVersion = 0x80000

-- | Generate an LLVM metadata node containing debug info for a
-- compilation unit.
compileUnitMetadata :: (Integral n1, Integral n2) => n1
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
                    -- ^ The list of descriptors for global variables.
                    -> IO ValueRef
                    -- ^ The LLVM Metadata for this file descriptor.
compileUnitMetadata lang file producer dir main opt flags
                    vers enums types subprogs gvars =
  do
    filemd <- mdString file
    dirmd <- mdString dir
    producermd <- mdString producer
    flagsmd <- mdString flags
    enummd <- mdNode enums
    typemd <- mdNode types
    subprogmd <- mdNode subprogs
    gvarmd <- mdNode gvars
    mdNode
      [ constInt int32Type (dwTag_compile_unit + llvmDebugVersion)
                      False,
        constInt int32Type (0 :: Word) False,
        constInt int32Type lang False,
        filemd, dirmd, producermd,
        constInt int1Type (if main then 1 else 0 :: Word) False,
        constInt int1Type (if opt then 1 else 0 :: Word) False,
        flagsmd,
        constInt int32Type vers False,
        enummd, typemd, subprogmd, gvarmd ]

-- | Generate an LLVM metadata node containing a file name.
fileMetadata :: String
             -- ^ The file name.
             -> String
             -- ^ The directory name.
             -> IO ValueRef
             -- ^ The LLVM Metadata for this file descriptor.
fileMetadata file dir =
  do
    namemd <- mdString file
    dirmd <- mdString dir
    empty <- mdNode []
    mdNode
      [ constInt int32Type (dwTag_variable + llvmDebugVersion) False,
        namemd, dirmd, empty ]

-- | Generate an LLVM metadata node describing a global variable declaration.
globalVarMetadata :: Integral n => ValueRef
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
                  -- ^ Whether or not this subprogram is local to the
                  -- compilation unit (static).
                  -> Bool
                  -- ^ Whether or not this subprogram is defined in the
                  -- compilation unit (not extern).
                  -> ValueRef
                  -- ^ A reference to the actual global variable itself.
                  -> IO ValueRef
                  -- ^ The LLVM Metadata for this global variable.
globalVarMetadata compunitmd name dispname linkname filemd lineno
                  typemd local defined ref =
  do
    namemd <- mdString name
    dispnamemd <- mdString dispname
    linknamemd <- mdString linkname
    mdNode
      [ constInt int32Type (dwTag_compile_unit + llvmDebugVersion) False,
        constInt int32Type (0 :: Word) False,
        compunitmd, namemd, dispnamemd, linknamemd, filemd,
        constInt int32Type lineno False,
        typemd,
        constInt int1Type (if local then 1 else 0 :: Word) False,
        constInt int1Type (if defined then 1 else 0 :: Word) False,
        ref ]

-- | Generate an LLVM metadata node containing information about a
-- subprogram (ie a function).
subprogramMetadata :: (Integral n1, Integral n2, Integral n3) => ValueRef
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
                   -- ^ Whether or not this subprogram is local to the
                   -- compilation unit (static).
                   -> Bool
                   -- ^ Whether or not this subprogram is defined in the
                   -- compilation unit (not extern).
                   -> ValueRef
                   -- ^ Indicates which base type contains the vtable
                   -- pointer for the derived class (from LLVM debug info
                   -- documentation).
                   -> n2
                   -- ^ Flags for the declaration.
                   -> Bool
                   -- ^ Whether or not this function is optimized.
                   -> ValueRef
                   -- ^ A reference to the actual function itself.
                   -> ValueRef
                   -- ^ A metadata containing the list of template parameters.
                   -> ValueRef
                   -- ^ A metadata describing the function declaration.
                   -> ValueRef
                   -- ^ A metadata containing the list of variables
                   -- declared in the function.
                   -> n3
                   -- ^ The beginning line number.
                   -> IO ValueRef
                   -- ^ The LLVM metadata for this function.
subprogramMetadata compunitmd dispname linkname filemd lineno typemd
                   local defined basetype flags optimized ref
                   tempparams funcdecl funcvars beginlineno =
  do
    dispnamemd <- mdString dispname
    linknamemd <- mdString linkname
    mdNode
      [ constInt int32Type (dwTag_subprogram + llvmDebugVersion) False,
        constInt int32Type (0 :: Word) False,
        compunitmd, dispnamemd, linknamemd, filemd,
        constInt int32Type lineno False,
        typemd,
        constInt int1Type (if local then 1 else 0 :: Word) False,
        constInt int1Type (if defined then 1 else 0 :: Word) False,
        basetype,
        constInt int32Type flags False,
        constInt int1Type (if optimized then 1 else 0 :: Word) False,
        ref, tempparams, funcdecl, funcvars,
        constInt int32Type beginlineno False ]

-- | Generate an LLVM metadata node containing debug information for a
-- basic block.
blockMetadata :: (Integral n1, Integral n2, Integral n3)
              => ValueRef
              -- ^ The metadata for the subprogram containing this block.
              -> n1
              -- ^ The line number.
              -> n2
              -- ^ The column number.
              -> ValueRef
              -- ^ The metadata for the file.
              -> n3
              -- ^ A unique identifier (for template instantiations).
              -> IO ValueRef
              -- ^ The metadata describing the block.
blockMetadata subprogrammd lineno colno filemd uid =
  mdNode
    [ constInt int32Type (dwTag_lexical_block + llvmDebugVersion) False,
      subprogrammd,
      constInt int32Type lineno False,
      constInt int32Type colno False,
      filemd,
      constInt int32Type uid False ]

-- | Generate an llvm metadata node containing debug information for a
-- basic type.
basicTypeMetadata :: (Integral n1, Integral n2, Integral n3,
                      Integral n4, Integral n5, Integral n6)
                  => ValueRef
                  -- ^ The metadata for the compilation unit declaring
                  -- this type.
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
                  -> IO ValueRef
                  -- ^ The LLVM metadata for this type.
basicTypeMetadata compunitmd name filemd lineno size
                  align offset flags encoding =
  do
    namemd <- mdString name
    mdNode
      [ constInt int32Type (dwTag_base_type + llvmDebugVersion) False,
        compunitmd, namemd, filemd,
        constInt int32Type lineno False,
        constInt int64Type size False,
        constInt int64Type align False,
        constInt int64Type offset False,
        constInt int32Type flags False,
        constInt int32Type encoding False ]

-- | Generate an LLVM metadata node containing debug information for a
-- composite type.
derivedTypeMetadata :: (Integral n1, Integral n2, Integral n3,
                        Integral n4, Integral n5, Integral n6)
                    => n1
                    -- ^ The tag.
                    -> ValueRef
                    -- ^ The metadata for the compilation unit declaring
                    -- this type.
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
                    -- ^ The metadata for the type from which this
                    -- is derived.
                    -> IO ValueRef
                    -- ^ The metadata for this type.
derivedTypeMetadata tag compunitmd name filemd lineno
                    size align offset flags derivemd =
  do
    namemd <- mdString name
    mdNode
      [ constInt int32Type ((fromIntegral tag) + llvmDebugVersion) False,
        compunitmd, compunitmd, namemd, filemd,
        constInt int32Type lineno False,
        constInt int64Type size False,
        constInt int64Type align False,
        constInt int64Type offset False,
        constInt int32Type flags False,
        derivemd ]

-- | Generate an LLVM metadata node containing debug information for a
-- composite type.
compositeTypeMetadata :: (Integral n1, Integral n2, Integral n3,
                          Integral n4, Integral n5, Integral n6)
                      => n1
                      -- ^ The tag
                      -> ValueRef
                      -- ^ The metadata for the compilation unit declaring
                      -- this type.
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
                      -- ^ The metadata for the type from which this is derived
                      -> ValueRef
                      -- ^ Metadata array containing member types
                      -> IO ValueRef
                      -- ^ The metadata for this type.
compositeTypeMetadata tag compunitmd name filemd lineno size
                      align offset flags derivemd membersmd =
  do
    namemd <- mdString name
    mdNode
      [ constInt int32Type ((fromIntegral tag) + llvmDebugVersion) False,
        compunitmd, compunitmd, namemd, filemd,
        constInt int32Type lineno False,
        constInt int64Type size False,
        constInt int64Type align False,
        constInt int64Type offset False,
        constInt int32Type flags False,
        derivemd, membersmd ]

-- | Generate an LLVM metadata node containing debug information for
-- an enum value
enumMetadata :: Integral n => String
             -- ^ The name.
             -> n
             -- ^ The enum value
             -> IO ValueRef
             -- ^ The metadata for this enum.
enumMetadata name value =
  do
    namemd <- mdString name
    mdNode
      [ constInt int32Type (dwTag_enumerator + llvmDebugVersion) False,
        namemd,
        constInt int32Type value False ]

-- | Generate an LLVM metadata node containing debug information for a
-- local variable.
localVarMetadata :: (Integral n1, Integral n2) => ValueRef
                 -- ^ The metadata for the lexical block declaring this
                 -- variable.
                 -> String
                 -- ^ The variable's name.
                 -> ValueRef
                 -- ^ The metadata for the file declaring this variable.
                 -> n1
                 -- ^ Line number where defined.
                 -> ValueRef
                 -- ^ The metadata for the type of this variable.
                 -> n2
                 -- ^ The flags.
                 -> IO ValueRef
                 -- ^ The LLVM metadata for this variable.
localVarMetadata blockmd name filemd lineno typemd flags =
  do
    namemd <- mdString name
    mdNode
      [ constInt int32Type (256 + llvmDebugVersion) False,
        blockmd, namemd, filemd,
        constInt int32Type lineno False,
        typemd,
        constInt int32Type flags False ]

-- | Generate an LLVM metadata node containing debug information for
-- an argument
argMetadata :: (Integral n1, Integral n2, Integral n3) => ValueRef
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
            -> IO ValueRef
            -- ^ The LLVM metadata for this variable
argMetadata blockmd name filemd lineno argno typemd flags =
  do
    namemd <- mdString name
    mdNode
      [ constInt int32Type (257 + llvmDebugVersion) False,
        blockmd, namemd, filemd,
        constInt int32Type ((shiftL (fromIntegral argno) 24) .|.
                                    (fromIntegral lineno) :: Word) False,
        typemd,
        constInt int32Type flags False ]

-- | Generate an LLVM metadata node giving a location.
locationMetadata :: (Integral n1, Integral n2) => n1
                 -- ^ The line number.
                 -> n2
                 -- ^ The column number.
                 -> ValueRef
                 -- ^ The metadata node for the lexical block containing
                 -- this.
                 -> IO ValueRef
locationMetadata lineno colno blockmd =
  mdNode
    [ constInt int32Type lineno False,
      constInt int32Type colno False,
      blockmd ]

-- | Generate an LLVM metadata node containing debug info for a
-- compilation unit.
compileUnitMetadataInContext :: (Integral n1, Integral n2) => ContextRef
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
                             -> [ValueRef]
                             -- ^ The list of descriptors for enum types.
                             -> [ValueRef]
                             -- ^ The list of descriptors for retained types.
                             -> [ValueRef]
                             -- ^ The list of descriptors for subprograms.
                             -> [ValueRef]
                             -- ^ The list of descriptors for global variables.
                             -> IO ValueRef
                             -- ^ The LLVM Metadata for this file descriptor.
compileUnitMetadataInContext ctx lang file producer dir main opt flags
                             vers enums types subprogs gvars =
  do
    int1ty <- int1TypeInContext ctx
    int32ty <- int32TypeInContext ctx
    filemd <- mdStringInContext ctx file
    dirmd <- mdStringInContext ctx dir
    producermd <- mdStringInContext ctx producer
    flagsmd <- mdStringInContext ctx flags
    enummd <- mdNodeInContext ctx enums
    typemd <- mdNodeInContext ctx types
    subprogmd <- mdNodeInContext ctx subprogs
    gvarmd <- mdNodeInContext ctx gvars
    mdNodeInContext ctx
      [ constInt int32ty (dwTag_compile_unit + llvmDebugVersion) False,
        constInt int32ty (0 :: Word) False,
        constInt int32ty lang False,
        filemd, dirmd, producermd,
        constInt int1ty (if main then 1 else 0 :: Word) False,
        constInt int1ty (if opt then 1 else 0 :: Word) False,
        flagsmd,
        constInt int32ty vers False,
        enummd, typemd, subprogmd, gvarmd ]

-- | Generate an LLVM metadata node containing a file name.
fileMetadataInContext :: ContextRef
                      -- ^ The LLVM context.
                      -> String
                      -- ^ The file name.
                      -> String
                      -- ^ The directory name.
                      -> IO ValueRef
                      -- ^ The LLVM Metadata for this file descriptor.
fileMetadataInContext ctx file dir =
  do
    namemd <- mdStringInContext ctx file
    dirmd <- mdStringInContext ctx dir
    int32ty <- int32TypeInContext ctx
    empty <- mdNodeInContext ctx []
    mdNodeInContext ctx
      [ constInt int32ty (dwTag_variable + llvmDebugVersion) False,
        namemd, dirmd, empty ]

-- | Generate an LLVM metadata node describing a global variable declaration.
globalVarMetadataInContext :: Integral n => ContextRef
                           -- ^ The LLVM context.
                           -> ValueRef
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
                           -- ^ Whether or not this subprogram is local to the
                           -- compilation unit (static).
                           -> Bool
                           -- ^ Whether or not this subprogram is defined in the
                           -- compilation unit (not extern).
                           -> ValueRef
                           -- ^ A reference to the actual global
                           -- variable itself.
                           -> IO ValueRef
                           -- ^ The LLVM Metadata for this global variable.
globalVarMetadataInContext ctx compunitmd name dispname linkname filemd lineno
                           typemd local defined ref =
  do
    int1ty <- int1TypeInContext ctx
    int32ty <- int32TypeInContext ctx
    namemd <- mdStringInContext ctx name
    dispnamemd <- mdStringInContext ctx dispname
    linknamemd <- mdStringInContext ctx linkname
    mdNodeInContext ctx
      [ constInt int32ty (dwTag_compile_unit + llvmDebugVersion) False,
        constInt int32ty (0 :: Word) False,
        compunitmd, namemd, dispnamemd, linknamemd, filemd,
        constInt int32ty lineno False,
        typemd,
        constInt int1ty (if local then 1 else 0 :: Word) False,
        constInt int1ty (if defined then 1 else 0 :: Word) False,
        ref ]

-- | Generate an LLVM metadata node containing information about a
-- subprogram (ie a function).
subprogramMetadataInContext :: (Integral n1, Integral n2, Integral n3)
                            => ContextRef
                            -- ^ The LLVM context.
                            -> ValueRef
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
                            -- ^ A metadata describing the function declaration.
                            -> ValueRef
                            -- ^ A metadata containing the list of variables
                            -- declared in the function.
                            -> n3
                            -- ^ The beginning line number.
                            -> IO ValueRef
                            -- ^ The LLVM metadata for this function.
subprogramMetadataInContext ctx compunitmd dispname linkname filemd lineno
                            typemd local defined basetype flags optimized ref
                            tempparams funcdecl funcvars beginlineno =
  do
    int1ty <- int1TypeInContext ctx
    int32ty <- int32TypeInContext ctx
    dispnamemd <- mdStringInContext ctx dispname
    linknamemd <- mdStringInContext ctx linkname
    mdNodeInContext ctx
      [ constInt int32ty (dwTag_subprogram + llvmDebugVersion) False,
        constInt int32ty (0 :: Word) False,
        compunitmd, dispnamemd, linknamemd, filemd,
        constInt int32ty lineno False,
        typemd,
        constInt int1ty (if local then 1 else 0 :: Word) False,
        constInt int1ty (if defined then 1 else 0 :: Word) False,
        basetype,
        constInt int32ty flags False,
        constInt int1ty (if optimized then 1 else 0 :: Word) False,
        ref, tempparams, funcdecl, funcvars,
        constInt int32ty beginlineno False ]

-- | Generate an LLVM metadata node containing debug information for a
-- basic block.
blockMetadataInContext :: (Integral n1, Integral n2, Integral n3)
                       => ContextRef
                       -- ^ The LLVM context.
                       -> ValueRef
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
                       -> IO ValueRef
                       -- ^ The metadata describing the block.
blockMetadataInContext ctx subprogrammd lineno colno filemd uid =
  do
    int32ty <- int32TypeInContext ctx
    mdNodeInContext ctx
      [ constInt int32ty (dwTag_lexical_block + llvmDebugVersion) False,
        subprogrammd,
        constInt int32ty lineno False,
        constInt int32ty colno False,
        filemd,
        constInt int32ty uid False ]

-- | Generate an llvm metadata node containing debug information for a
-- basic type.
basicTypeMetadataInContext :: (Integral n1, Integral n2, Integral n3,
                               Integral n4, Integral n5, Integral n6)
                           => ContextRef
                           -- ^ The LLVM context.
                           -> ValueRef
                           -- ^ The metadata for the compilation unit declaring
                           -- this type.
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
                           -> IO ValueRef
                           -- ^ The LLVM metadata for this type.
basicTypeMetadataInContext ctx compunitmd name filemd lineno size
                           align offset flags encoding =
  do
    int32ty <- int32TypeInContext ctx
    int64ty <- int64TypeInContext ctx
    namemd <- mdStringInContext ctx name
    mdNodeInContext ctx
      [ constInt int32ty (dwTag_base_type + llvmDebugVersion) False,
        compunitmd, namemd, filemd,
        constInt int32ty lineno False,
        constInt int64ty size False,
        constInt int64ty align False,
        constInt int64ty offset False,
        constInt int32ty flags False,
        constInt int32ty encoding False ]

-- | Generate an LLVM metadata node containing debug information for a
-- composite type.
derivedTypeMetadataInContext :: (Integral n1, Integral n2, Integral n3,
                                 Integral n4, Integral n5, Integral n6)
                             => ContextRef
                             -- ^ The LLVM context.
                             -> n1
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
                             -> IO ValueRef
                             -- ^ The metadata for this type.
derivedTypeMetadataInContext ctx tag compunitmd name filemd lineno
                             size align offset flags derivemd =
  do
    int64ty <- int64TypeInContext ctx
    int32ty <- int32TypeInContext ctx
    namemd <- mdStringInContext ctx name
    mdNodeInContext ctx
      [ constInt int32ty ((fromIntegral tag) + llvmDebugVersion) False,
        compunitmd, compunitmd, namemd, filemd,
        constInt int32ty lineno False,
        constInt int64ty size False,
        constInt int64ty align False,
        constInt int64ty offset False,
        constInt int32ty flags False,
        derivemd ]

-- | Generate an LLVM metadata node containing debug information for a
-- composite type.
compositeTypeMetadataInContext :: (Integral n1, Integral n2, Integral n3,
                                   Integral n4, Integral n5, Integral n6)
                               => ContextRef
                               -- ^ The LLVM context.
                               -> n1
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
                               -> IO ValueRef
                               -- ^ The metadata for this type.
compositeTypeMetadataInContext ctx tag compunitmd name filemd lineno size
                               align offset flags derivemd membersmd =
  do
    int64ty <- int64TypeInContext ctx
    int32ty <- int32TypeInContext ctx
    namemd <- mdStringInContext ctx name
    mdNodeInContext ctx
      [ constInt int32ty ((fromIntegral tag) + llvmDebugVersion) False,
        compunitmd, compunitmd, namemd, filemd,
        constInt int32ty lineno False,
        constInt int64ty size False,
        constInt int64ty align False,
        constInt int64ty offset False,
        constInt int32ty flags False,
        derivemd, membersmd ]

-- | Generate an LLVM metadata node containing debug information for
-- an enum value
enumMetadataInContext :: Integral n => ContextRef
                      -- ^ The LLVM context.
                      -> String
                      -- ^ The name.
                      -> n
                      -- ^ The enum value
                      -> IO ValueRef
                      -- ^ The metadata for this enum.
enumMetadataInContext ctx name value =
  do
    int32ty <- int32TypeInContext ctx
    namemd <- mdStringInContext ctx name
    mdNodeInContext ctx
      [ constInt int32ty (dwTag_enumerator + llvmDebugVersion) False,
        namemd,
        constInt int32ty value False ]

-- | Generate an LLVM metadata node containing debug information for a
-- local variable.
localVarMetadataInContext :: (Integral n1, Integral n2) => ContextRef
                          -- ^ The LLVM context.
                          -> ValueRef
                          -- ^ The metadata for the lexical block declaring this
                          -- variable.
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
                          -> IO ValueRef
                          -- ^ The LLVM metadata for this variable.
localVarMetadataInContext ctx blockmd name filemd lineno typemd flags =
  do
    int32ty <- int32TypeInContext ctx
    namemd <- mdStringInContext ctx name
    mdNodeInContext ctx
      [ constInt int32ty (256 + llvmDebugVersion) False,
        blockmd, namemd, filemd,
        constInt int32ty lineno False,
        typemd,
        constInt int32ty flags False ]

-- | Generate an LLVM metadata node containing debug information for
-- an argument
argMetadataInContext :: (Integral n1, Integral n2, Integral n3) => ContextRef
                     -- ^ The LLVM Context
                     -> ValueRef
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
                     -> IO ValueRef
                     -- ^ The LLVM metadata for this variable
argMetadataInContext ctx blockmd name filemd lineno argno typemd flags =
  do
    int32ty <- int32TypeInContext ctx
    namemd <- mdStringInContext ctx name
    mdNodeInContext ctx
      [ constInt int32ty (257 + llvmDebugVersion) False,
        blockmd, namemd, filemd,
        constInt int32ty ((shiftL (fromIntegral argno) 24) .|.
                               (fromIntegral lineno) :: Word) False,
        typemd,
        constInt int32ty flags False ]

-- | Generate an LLVM metadata node giving a location.
locationMetadataInContext :: (Integral n1, Integral n2) => ContextRef
                          -- ^ The LLVM Context.
                          -> n1
                          -- ^ The line number.
                          -> n2
                          -- ^ The column number.
                          -> ValueRef
                          -- ^ The metadata node for the lexical block
                          -- containing this.
                          -> IO ValueRef
locationMetadataInContext ctx lineno colno blockmd =
  do
    int32ty <- int32TypeInContext ctx
    mdNodeInContext ctx
      [ constInt int32ty lineno False,
        constInt int32ty colno False,
        blockmd ]
