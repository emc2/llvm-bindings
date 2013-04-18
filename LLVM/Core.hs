-- Copyright (c) 2013 Eric McCorkle.  All rights reserved.
--
-- Portions of this code are derived from software originally written
-- by Brian O' Sullivan.  Comments are copied in part from software
-- produced by the LLVM Compiler Infrastructure project.
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

-- | Bindings for llvm-c/Core.h, wrapped in utility code to make them
-- more usable for general purposes.
module LLVM.Core(
       -- * Types
       ContextRef,
       ModuleRef,
       TypeRef,
       ValueRef,
       BasicBlockRef,
       BuilderRef,
       MemoryBufferRef,
       PassManagerRef,
       PassRegistryRef,
       UseRef,
       Opcode(..),
       Attribute(..),
       TypeKind(..),
       Linkage(..),
       Visibility(..),
       IntPredicate(..),
       RealPredicate(..),
       CallingConvention(..),

       -- * Error handling
       disposeMessage,

       -- * Context functions.
       -- | Contexts are execution states for the core LLVM IR system.

       -- | Most types are tied to a context instance. Multiple contexts
       -- can exist simultaneously. A single context is not thread
       -- safe. However, different contexts can execute on different
       -- threads simultaneously.

       contextCreate,
       contextDispose,
       getGlobalContext,
       getMDKindID,
       getMDKindIDInContext,

       -- * Modules
       -- | Modules represent the top-level structure in a LLVM
       -- program. An LLVM module is effectively a translation unit or
       -- a collection of translation units merged together.

       moduleCreateWithName,
       moduleCreateWithNameInContext,
       disposeModule,

       -- ** Data Layout
       getDataLayout,
       setDataLayout,

       -- ** Targets
       getTarget,
       setTarget,

       -- ** Dump module
       dumpModule,

       -- ** Types
       getTypeByName,

       -- ** Metadata
       getNamedMetadataNumOperands,
       getNamedMetadataOperands,
       addNamedMetadataOperand,

       -- ** Functions
       addFunction,
       getNamedFunction,
       getFirstFunction,
       getLastFunction,
       getNextFunction,
       getPreviousFunction,

       -- ** Other module functions
       setModuleInlineAsm,
       getModuleContext,

       -- * Types
       -- | Types represent the type of a value.
       --
       --  Types are associated with a context instance. The context
       --  internally deduplicates types so there is only 1 instance
       --  of a specific type alive at a time. In other words, a
       --  unique type is shared among all consumers within a context.

       getTypeKind,
       typeIsSized,
       getTypeContext,

       -- ** Integer types
       -- | Functions in this section operate on integer types.

       int1TypeInContext,
       int8TypeInContext,
       int16TypeInContext,
       int32TypeInContext,
       int64TypeInContext,
       intTypeInContext,
       int1Type,
       int8Type,
       int16Type,
       int32Type,
       int64Type,
       intType,
       getIntTypeWidth,

       -- ** Floating point types
       -- | Functions in this section operate on integer types.
       floatTypeInContext,
       doubleTypeInContext,
       x86FP80TypeInContext,
       fp128TypeInContext,
       ppcFP128TypeInContext,
       floatType,
       doubleType,
       x86FP80Type,
       fp128Type,
       ppcFP128Type,

       -- ** Function types
       -- | These functions relate to function type instances.

       functionType,
       isFunctionVarArg,
       getReturnType,
       countParamTypes,
       getParamTypes,

       -- ** Struct types
       -- | These functions relate to structure type instances.

       structTypeInContext,
       structType,
       structCreateNamed,
       getStructName,
       structSetBody,
       countStructElementTypes,
       getStructElementTypes,
       isPackedStruct,
       isOpaqueStruct,

       -- ** Array, pointer, and vector types
       -- | Sequential types represents "arrays" of types. This is a
       -- super class for array, vector, and pointer types.

       arrayType,
       pointerType,
       vectorType,
       getElementType,
       getArrayLength,
       getPointerAddressSpace,
       getVectorSize,

       -- ** Other types
       voidTypeInContext,
       labelTypeInContext,
       x86MMXTypeInContext,
       voidType,
       labelType,
       x86MMXType,

       -- * Values
       -- | Functions in this section work on all LLVMValueRef
       -- instances, regardless of their sub-type. They correspond to
       -- functions available on llvm::Value.

       isAArgument,
       isABasicBlock,
       isAInlineAsm,
       isAMDNode,
       isAMDString,
       isAUser,
       isAConstant,
       isABlockAddress,
       isAConstantAggregateZero,
       isAConstantArray,
       isAConstantExpr,
       isAConstantFP,
       isAConstantInt,
       isAConstantPointerNull,
       isAConstantStruct,
       isAConstantVector,
       isAGlobalValue,
       isAFunction,
       isAGlobalAlias,
       isAGlobalVariable,
       isAUndefValue,
       isAInstruction,
       isABinaryOperator,
       isACallInst,
       isAIntrinsicInst,
       isADbgInfoIntrinsic,
       isADbgDeclareInst,
       isAMemIntrinsic,
       isAMemCpyInst,
       isAMemMoveInst,
       isAMemSetInst,
       isACmpInst,
       isAFCmpInst,
       isAICmpInst,
       isAExtractElementInst,
       isAGetElementPtrInst,
       isAInsertElementInst,
       isAInsertValueInst,
       isALandingPadInst,
       isAPHINode,
       isASelectInst,
       isAShuffleVectorInst,
       isAStoreInst,
       isATerminatorInst,
       isABranchInst,
       isAIndirectBrInst,
       isAInvokeInst,
       isAReturnInst,
       isASwitchInst,
       isAUnreachableInst,
       isAResumeInst,
       isAUnaryInstruction,
       isAAllocaInst,
       isACastInst,
       isABitCastInst,
       isAFPExtInst,
       isAFPToSIInst,
       isAFPToUIInst,
       isAFPTruncInst,
       isAIntToPtrInst,
       isAPtrToIntInst,
       isASExtInst,
       isASIToFPInst,
       isATruncInst,
       isAUIToFPInst,
       isAZExtInst,
       isAExtractValueInst,
       isALoadInst,
       isAVAArgInst,
       typeOf,
       getValueName,
       setValueName,
       dumpValue,
       replaceAllUsesWith,
       hasMetadata,
       getMetadata,
       setMetadata,

       -- ** Uses
       -- | This section contains functions that allow you to inspect
       -- the uses of a LLVMValueRef.
       --
       -- It is possible to obtain a LLVMUseRef for any LLVMValueRef
       -- instance.  Each LLVMUseRef (which corresponds to a llvm::Use
       -- instance) holds a llvm::User and llvm::Value.

       getFirstUse,
       getNextUse,
       getUser,
       getUsedValue,

       -- ** Users
       -- | Functions in this group pertain to LLVMValueRef instances
       -- that descent from llvm::User. This includes constants,
       -- instructions, and operators.

       getOperand,
       setOperand,
       getNumOperands,

       -- * Constants
       -- | This section contains functions for interacting with
       -- LLVMValueRef that correspond to llvm::Constant instances.
       --
       -- These functions will work for any LLVMValueRef in the
       -- llvm::Constant class hierarchy.

       constNull,
       constAllOnes,
       getUndef,
       isConstant,
       isNull,
       isUndef,
       constPointerNull,

       -- ** Scalar constants
       -- | Functions in this group model LLVMValueRef instances that
       -- correspond to constants referring to scalar types.
       --
       -- For integer types, the LLVMTypeRef parameter should
       -- correspond to a llvm::IntegerType instance and the returned
       -- LLVMValueRef will correspond to a llvm::ConstantInt.
       --
       -- For floating point types, the LLVMTypeRef returned
       -- corresponds to a llvm::ConstantFP.

       constInt,
       constIntOfArbitraryPrecision,
       constIntOfString,
       constReal,
       constRealOfString,
       constIntGetZExtValue,
       constIntGetSExtValue,

       -- ** Composite constants
       -- | Functions in this group operate on composite constants.

       constStringInContext,
       constStructInContext,
       constString,
       constArray,
       constStruct,
       constNamedStruct,
       constVector,

       -- ** Constant Expressions
       -- | Functions in this group correspond to APIs on llvm::ConstantExpr.

       getConstOpcode,
       alignOf,
       sizeOf,
       constNeg,
       constNUWNeg,
       constNSWNeg,
       constFNeg,
       constNot,
       constAdd,
       constNSWAdd,
       constNUWAdd,
       constFAdd,
       constSub,
       constNSWSub,
       constNUWSub,
       constFSub,
       constMul,
       constNSWMul,
       constNUWMul,
       constFMul,
       constUDiv,
       constSDiv,
       constExactSDiv,
       constFDiv,
       constURem,
       constSRem,
       constFRem,
       constAnd,
       constOr,
       constXor,
       constICmp,
       constFCmp,
       constShl,
       constLShr,
       constAShr,
       constGEP,
       constInBoundsGEP,
       constTrunc,
       constSExt,
       constZExt,
       constFPTrunc,
       constFPExt,
       constUIToFP,
       constSIToFP,
       constFPToUI,
       constFPToSI,
       constPtrToInt,
       constIntToPtr,
       constBitCast,
       constZExtOrBitCast,
       constSExtOrBitCast,
       constTruncOrBitCast,
       constPointerCast,
       constIntCast,
       constFPCast,
       constSelect,
       constExtractElement,
       constInsertElement,
       constShuffleVector,
       constExtractValue,
       constInsertValue,
       constInlineAsm,
       blockAddress,

       -- * Global variables, functions, and aliases (globals)
       -- | This group contains functions that operate on global
       -- values. Functions in this group relate to functions in the
       -- llvm::GlobalValue class tree.

       getGlobalParent,
       isDeclaration,
       getLinkage,
       setLinkage,
       getSection,
       setSection,
       getVisibility,
       setVisibility,
       getAlignment,
       setAlignment,

       -- ** Global variables
       -- | This group contains functions that operate on global
       -- variable values.

       addGlobal,
       addGlobalInAddressSpace,
       getNamedGlobal,
       getFirstGlobal,
       getLastGlobal,
       getNextGlobal,
       getPreviousGlobal,
       deleteGlobal,
       getInitializer,
       setInitializer,
       isThreadLocal,
       setThreadLocal,
       isGlobalConstant,
       setGlobalConstant,

       -- ** Aliases
       -- | This group contains function that operate on global alias
       -- values.

       addAlias,

       -- ** Functions
       -- | Functions in this group operate on LLVMValueRef instances
       -- that correspond to llvm::Function instances.

       deleteFunction,
       getIntrinsicID,
       getFunctionCallConv,
       setFunctionCallConv,
       getGC,
       setGC,
       addFunctionAttr,
       removeFunctionAttr,

       -- ** Parameters
       -- | Functions in this group relate to arguments/parameters on
       -- functions.
       --
       -- Functions in this group expect LLVMValueRef instances that
       -- correspond to llvm::Function instances.

       countParams,
       getParams,
       getParam,
       getParamParent,
       getFirstParam,
       getLastParam,
       getNextParam,
       getPreviousParam,
       addAttribute,
       removeAttribute,
       setParamAlignment,

       -- ** Metadata
       mdStringInContext,
       mdString,
       mdNodeInContext,
       mdNode,
       getMDString,

       -- ** Basic blocks
       -- | A basic block represents a single entry single exit
       -- section of code.  Basic blocks contain a list of
       -- instructions which form the body of the block.
       --
       -- Basic blocks belong to functions. They have the type of
       -- label.
       --
       -- Basic blocks are themselves values. However, the C API
       -- models them as LLVMBasicBlockRef.

       basicBlockAsValue,
       valueIsBasicBlock,
       valueAsBasicBlock,
       getBasicBlockParent,
       getBasicBlockTerminator,
       countBasicBlocks,
       getBasicBlocks,
       getFirstBasicBlock,
       getLastBasicBlock,
       getNextBasicBlock,
       getPreviousBasicBlock,
       getEntryBasicBlock,
       appendBasicBlockInContext,
       insertBasicBlockInContext,
       appendBasicBlock,
       insertBasicBlock,
       deleteBasicBlock,
       removeBasicBlockFromParent,
       moveBasicBlockBefore,
       moveBasicBlockAfter,
       getFirstInstruction,
       getLastInstruction,

       -- ** Instructions
       -- | Functions in this group relate to the inspection and
       -- manipulation of individual instructions.
       --
       -- In the C++ API, an instruction is modeled by
       -- llvm::Instruction. This class has a large number of
       -- descendents. llvm::Instruction is a llvm::Value and in the C
       -- API, instructions are modeled by LLVMValueRef.
       --
       -- This group also contains sub-groups which operate on
       -- specific llvm::Instruction types, e.g. llvm::CallInst.

       getInstructionParent,
       getNextInstruction,
       getPreviousInstruction,
       instructionEraseFromParent,
       getInstructionOpcode,
       getICmpPredicate,

       -- ** Call Sites
       -- | Functions in this group apply to instructions that refer
       -- to call sites and invocations. These correspond to C++ types
       -- in the llvm::CallInst class tree.

       getInstructionCallConv,
       setInstructionCallConv,
       addInstrAttribute,
       removeInstrAttribute,
       setInstrParamAlignment,

       -- ** Call Instructions (only)
       isTailCall,
       setTailCall,

       -- ** Switch Instructions (only)
       getSwitchDefaultDest,

       -- ** Phi nodes
       addIncoming,
       countIncoming,
       getIncomingValue,
       getIncomingBlock,

       -- * Instruction building
       -- | An instruction builder represents a point within a basic
       -- block and is the exclusive means of building instructions
       -- using the C interface.

       createBuilderInContext,
       createBuilder,
       positionBuilder,
       positionBefore,
       positionAtEnd,
       getInsertBlock,
       clearInsertionPosition,
       insertIntoBuilder,
       insertIntoBuilderWithName,
       disposeBuilder,

       -- ** Metadata
       setCurrentDebugLocation,
       getCurrentDebugLocation,
       setInstDebugLocation,

       -- ** Terminators
       buildRetVoid,
       buildRet,
       buildAggregateRet,
       buildBr,
       buildCondBr,
       buildSwitch,
       buildIndirectBr,
       buildInvoke,
       buildLandingPad,
       buildResume,
       buildUnreachable,

       addCase,
       addDestination,
       addClause,
       setCleanup,

       -- ** Arithmetic
       buildAdd,
       buildNSWAdd,
       buildNUWAdd,
       buildFAdd,
       buildSub,
       buildNSWSub,
       buildNUWSub,
       buildFSub,
       buildMul,
       buildNSWMul,
       buildNUWMul,
       buildFMul,
       buildUDiv,
       buildSDiv,
       buildExactSDiv,
       buildFDiv,
       buildURem,
       buildSRem,
       buildFRem,
       buildShl,
       buildLShr,
       buildAShr,
       buildAnd,
       buildOr,
       buildXor,
       buildBinOp,
       buildNeg,
       buildNSWNeg,
       buildNUWNeg,
       buildFNeg,
       buildNot,

       -- ** Memory
       buildMalloc,
       buildArrayMalloc,
       buildAlloca,
       buildArrayAlloca,
       buildFree,
       buildLoad,
       buildStore,
       buildGEP,
       buildInBoundsGEP,
       buildStructGEP,
       buildGlobalString,
       buildGlobalStringPtr,
       getVolatile,
       setVolatile,

       -- ** Casts
       buildTrunc,
       buildZExt,
       buildSExt,
       buildFPToUI,
       buildFPToSI,
       buildUIToFP,
       buildSIToFP,
       buildFPTrunc,
       buildFPExt,
       buildPtrToInt,
       buildIntToPtr,
       buildBitCast,
       buildZExtOrBitCast,
       buildSExtOrBitCast,
       buildTruncOrBitCast,
       buildCast,
       buildPointerCast,
       buildIntCast,
       buildFPCast,

       -- ** Comparisons
       buildICmp,
       buildFCmp,

       -- ** Miscellaneous instructions
       buildPhi,
       buildCall,
       buildSelect,
       buildVAArg,
       buildExtractElement,
       buildInsertElement,
       buildShuffleVector,
       buildExtractValue,
       buildInsertValue,
       buildIsNull,
       buildIsNotNull,
       buildPtrDiff,

       -- * Memory buffers
       createMemoryBufferWithContentsOfFile,
       createMemoryBufferWithSTDIN,
       disposeMemoryBuffer,

       -- * PassRegistry
       getGlobalPassRegistry,

       -- * Pass manager
       createPassManager,
       createFunctionPassManagerForModule,
       createFunctionPassManager,
       runPassManager,
       initializeFunctionPassManager,
       runFunctionPassManager,
       finalizeFunctionPassManager,
       disposePassManager
       ) where

import Foreign hiding (unsafePerformIO, sizeOf)
import Foreign.C.String
import Foreign.C.Types
import LLVM.FFI.Core(ContextRef, ModuleRef, TypeRef, ValueRef, UseRef,
                     BasicBlockRef, BuilderRef, MemoryBufferRef,
                     PassManagerRef, PassRegistryRef, ModuleProviderRef,
                     Attribute(..), TypeKind(..), Linkage(..), Visibility(..),
                     IntPredicate(..), RealPredicate(..),
                     CallingConvention(..), Opcode(..))
import System.IO.Unsafe

import qualified LLVM.FFI.Core as FFI

disposeMessage = FFI.disposeMessage

-- * Contexts

-- | Create a new context.
--
-- Every call to this function should be paired with a call to
-- contextDispose or the context will leak memory.
contextCreate :: IO ContextRef
contextCreate = FFI.contextCreate

-- | Obtain the global context instance.
getGlobalContext :: IO ContextRef
getGlobalContext = FFI.getGlobalContext

-- | Destroy a context instance.
--
-- This should be called for every call to contextCreate() or memory
-- will be leaked.
contextDispose = FFI.contextDispose
contextDispose :: ContextRef
               -- ^ Context to destroy.
               -> IO ()

-- | Get the kind ID for a metadata tag.  This is used to attach
-- specific metadata to an instruction.  For example, look up "dbg" to
-- get the kind ID used to attach !dbg metadata.
getMDKindIDInContext :: Num n
                     => ContextRef
                     -- ^ Context
                     -> String
                     -- ^ Metadata name
                     -> IO n
getMDKindIDInContext ctx str =
  withCString str
    (\cstr ->
      FFI.getMDKindIDInContext ctx cstr (fromIntegral (length str)) >>=
        return . fromIntegral)

getMDKindID :: Num n
            => String
            -- ^ Metadata name
            -> IO n
getMDKindID str =
  withCString str
    (\cstr ->
      FFI.getMDKindID cstr (fromIntegral (length str)) >>=
        return . fromIntegral)

-- * Modules

-- | Create a new, empty module in the global context.
--
-- This is equivalent to calling moduleCreateWithNameInContext with
-- gGetGlobalContext() as the context parameter.
--
-- Every invocation should be paired with disposeModule() or memory
-- will be leaked.
moduleCreateWithName :: String
                     -- ^ Module name
                     -> IO ModuleRef
moduleCreateWithName str = withCString str FFI.moduleCreateWithName

-- | Create a new, empty module in a specific context.
--
-- Every invocation should be paired with disposeModule() or memory
-- will be leaked.
moduleCreateWithNameInContext :: String
                                -- ^ Module name
                              -> ContextRef
                                -- ^ Context
                              -> IO ModuleRef
moduleCreateWithNameInContext str ctx =
  withCString str (\cstr -> FFI.moduleCreateWithNameInContext cstr ctx)

-- | Destroy a module instance.
--
-- This must be called for every created module or memory will be
-- leaked.
disposeModule :: ModuleRef
              -- ^ Module to destroy
              -> IO ()
disposeModule = FFI.disposeModule

-- ** Data Layout

-- | Obtain the data layout for a module.
getDataLayout :: ModuleRef
              -- ^ Module
              -> IO String
              -- ^ Data layout string
getDataLayout mod = FFI.getDataLayout mod >>= peekCString

-- | Set the data layout for a module.  See Module::getDataLayout()
setDataLayout :: ModuleRef
              -- ^ Module
              -> String
              -- ^ Data layout string
              -> IO ()
setDataLayout mod str = withCString str (FFI.setDataLayout mod)

-- ** Targets

-- | Obtain the target triple for a module.
getTarget :: ModuleRef
          -- ^ Module
          -> IO String
          -- ^ Target triple string
getTarget mod = FFI.getTarget mod >>= peekCString

-- | Set the target triple for a module.
setTarget :: ModuleRef
          -- ^ Module
          -> String
          -- ^ Target triple string
          -> IO ()
setTarget mod str = withCString str (FFI.setTarget mod)

-- ** Output

-- | Dump a representation of a module to stderr.
dumpModule :: ModuleRef -> IO ()
dumpModule = FFI.dumpModule

-- ** Types

-- | Obtain a Type from a module by its registered name.
getTypeByName :: ModuleRef
              -- ^ Module
              -> String
              -- ^ Type name
              -> IO TypeRef
getTypeByName mod str = withCString str (FFI.getTypeByName mod)

-- ** Metadata

-- | Obtain the number of operands for named metadata in a module.
getNamedMetadataNumOperands :: Integral n => ModuleRef
                            -- ^ Module
                            -> String
                            -- ^ Metadata name
                            -> IO n
                            -- ^ Number of operands
getNamedMetadataNumOperands mod str =
  withCString str
    (\cstr -> FFI.getNamedMetadataNumOperands mod cstr >>=
                return . fromIntegral)

-- | Obtain the named metadata operands for a module.
getNamedMetadataOperands :: ModuleRef
                         -- ^ Module
                         -> String
                         -- ^ Metadata name
                         -> IO [ValueRef]
                         -- ^ Operands
getNamedMetadataOperands mod str =
  do
    len <- getNamedMetadataNumOperands mod str
    allocaArray (fromIntegral len)
      (\arr -> withCString str
        (\cstr -> FFI.getNamedMetadataOperands mod cstr arr >>
                    peekArray len arr))

-- | Add an operand to named metadata.
addNamedMetadataOperand :: ModuleRef
                        -- ^ Module
                        -> String
                        -- ^ Metadata name
                        -> ValueRef
                        -- ^ Operand (must be a metadata node)
                        -> IO ()
addNamedMetadataOperand mod str val =
  withCString str (\cstr -> FFI.addNamedMetadataOperand mod cstr val)

-- ** Functions

-- | Add a function to a module under a specified name.
addFunction :: ModuleRef
            -- ^ Module
            -> String
            -- ^ Function name
            -> TypeRef
            -- ^ Function type
            -> IO ValueRef
            -- ^ Function value
addFunction mod str ty = withCString str (\cstr -> FFI.addFunction mod cstr ty)

-- | Obtain a Function value from a Module by its name.
--
-- The returned value corresponds to a llvm::Function value.
getNamedFunction :: ModuleRef
                 -- ^ Module
                 -> String
                 -- ^ Function name
                 -> IO ValueRef
                 -- ^ Function value (@nullPtr@ if not found)
getNamedFunction mod str = withCString str (FFI.getNamedFunction mod)

-- | Obtain an iterator to the first Function in a Module.
getFirstFunction :: ModuleRef -> IO ValueRef
getFirstFunction = FFI.getFirstFunction

-- | Obtain an iterator to the last Function in a Module.
getLastFunction :: ModuleRef -> IO ValueRef
getLastFunction = FFI.getLastFunction

-- | Advance a Function iterator to the next Function.
--
-- Returns @nullPtr@ if the iterator was already at the end and there
-- are no more functions.
getNextFunction :: ValueRef -> IO ValueRef
getNextFunction = FFI.getNextFunction

-- | Decrement a Function iterator to the previous Function.
--
-- Returns @nullPtr@ if the iterator was already at the beginning and
-- there are no previous functions.
getPreviousFunction :: ValueRef -> IO ValueRef
getPreviousFunction = FFI.getPreviousFunction

-- ** Other module functions

-- | Obtain the context to which this module is associated.
getModuleContext :: ModuleRef -> IO ContextRef
getModuleContext = FFI.getModuleContext

-- | Set inline assembly for a module.
setModuleInlineAsm :: ModuleRef
                   -- ^ Module
                   -> String
                   -- ^ Assembly string
                   -> IO ()
setModuleInlineAsm mod str = withCString str (FFI.setModuleInlineAsm mod)

-- * Types

-- | Obtain the enumerated type of a Type instance.
getTypeKind :: TypeRef -> IO TypeKind
getTypeKind ty = FFI.getTypeKind ty >>= return . FFI.toTypeKind

-- | Whether the type has a known size.
--
-- Things that don't have a size are abstract types, labels, and void.
typeIsSized :: TypeRef -> IO Bool
typeIsSized ty = FFI.typeIsSized ty >>= return . toBool

-- | Obtain the context to which this type instance is associated.
getTypeContext :: TypeRef -> IO ContextRef
getTypeContext = FFI.getTypeContext

-- ** Integer types

int1TypeInContext :: ContextRef -> IO TypeRef
int1TypeInContext = FFI.int1TypeInContext

int8TypeInContext :: ContextRef -> IO TypeRef
int8TypeInContext = FFI.int8TypeInContext

int16TypeInContext :: ContextRef -> IO TypeRef
int16TypeInContext = FFI.int16TypeInContext

int32TypeInContext :: ContextRef -> IO TypeRef
int32TypeInContext = FFI.int32TypeInContext

int64TypeInContext :: ContextRef -> IO TypeRef
int64TypeInContext = FFI.int64TypeInContext

intTypeInContext :: Integral n => ContextRef -> n -> IO TypeRef
intTypeInContext ctx = FFI.intTypeInContext ctx . fromIntegral

int1Type :: TypeRef
int1Type = FFI.int1Type

int8Type :: TypeRef
int8Type = FFI.int8Type

int16Type :: TypeRef
int16Type = FFI.int16Type

int32Type :: TypeRef
int32Type = FFI.int32Type

int64Type :: TypeRef
int64Type = FFI.int64Type

intType :: Integral n => n -> TypeRef
intType = FFI.intType . fromIntegral

getIntTypeWidth :: Num n => TypeRef -> IO n
getIntTypeWidth tyref = FFI.getIntTypeWidth tyref >>= return . fromIntegral

-- ** Floating point types
floatTypeInContext :: ContextRef -> IO TypeRef
floatTypeInContext = FFI.floatTypeInContext

doubleTypeInContext :: ContextRef -> IO TypeRef
doubleTypeInContext = FFI.doubleTypeInContext

x86FP80TypeInContext :: ContextRef -> IO TypeRef
x86FP80TypeInContext = FFI.x86FP80TypeInContext

fp128TypeInContext :: ContextRef -> IO TypeRef
fp128TypeInContext = FFI.fp128TypeInContext

ppcFP128TypeInContext :: ContextRef -> IO TypeRef
ppcFP128TypeInContext = FFI.ppcFP128TypeInContext

floatType :: TypeRef
floatType = FFI.floatType

doubleType :: TypeRef
doubleType = FFI.doubleType

x86FP80Type :: TypeRef
x86FP80Type = FFI.x86FP80Type

fp128Type :: TypeRef
fp128Type = FFI.fp128Type

ppcFP128Type :: TypeRef
ppcFP128Type = FFI.ppcFP128Type

-- ** Function types

-- | Obtain a function type consisting of a specified signature.
--
-- The function is defined as a tuple of a return Type, a list of
-- parameter types, and whether the function is variadic.
functionType :: TypeRef
             -- ^ Return type
             -> [TypeRef]
             -- ^ Parameter types
             -> Bool
             -- ^ Variable arguments
             -> TypeRef
             -- ^ The function type
functionType retty params vararg =
  unsafePerformIO
    (withArrayLen params
      (\len arr ->
        return (FFI.functionType retty arr (fromIntegral len)
                                 (fromBool vararg))))

-- | Returns whether a function type is variadic.
isFunctionVarArg :: TypeRef -> IO Bool
isFunctionVarArg func = FFI.isFunctionVarArg func >>= return . toBool

-- | Obtain the Type this function Type returns.
getReturnType :: TypeRef -> IO TypeRef
getReturnType = FFI.getReturnType

-- | Obtain the number of parameters this function accepts.
countParamTypes :: Num n => TypeRef -> IO n
countParamTypes func = FFI.countParamTypes func >>= return . fromIntegral

-- | Obtain the types of a function's parameters.
getParamTypes :: TypeRef
              -- ^ Function type
              -> IO [TypeRef]
              -- ^ All parameters
getParamTypes func =
  do
    len <- countParamTypes func
    allocaArray len (\arr -> FFI.getParamTypes func arr >> peekArray len arr)

-- | Create a new structure type in a context.
--
-- A structure is specified by a list of inner elements/types and
-- whether these can be packed together.
structTypeInContext :: ContextRef
                    -- ^ Context
                    -> [TypeRef]
                    -- ^ Element types
                    -> Bool
                    -- ^ True if the structure is packed
                    -> IO TypeRef
                    -- ^ Structure type
structTypeInContext ctx fields packed =
  withArrayLen fields
    (\len arr -> FFI.structTypeInContext ctx arr (fromIntegral len)
                                         (fromBool packed))

-- | Create a new structure type in the global context.
structType :: [TypeRef]
           -- ^ Element types
           -> Bool
           -- ^ True if the structure is packed
           -> TypeRef
           -- ^ Structure type
structType fields packed =
  unsafePerformIO
    (withArrayLen fields
      (\len arr ->
        return (FFI.structType arr (fromIntegral len) (fromBool packed))))

-- | Create an empty structure in a context having a specified name.
structCreateNamed :: ContextRef
                  -- ^ Context
                  -> String
                  -- ^ Structure name
                  -> IO TypeRef
                  -- ^ Structure type
structCreateNamed ctx str = withCString str (FFI.structCreateNamed ctx)

-- | Obtain the name of a structure.
getStructName :: TypeRef -> String
getStructName ty = unsafePerformIO (peekCString (FFI.getStructName ty))

-- | Set the contents of a structure type.
structSetBody :: TypeRef
              -- ^ Structure type
              -> [TypeRef]
              -- ^ Element types
              -> Bool
              -- ^ Whether the structure is packed
              -> IO ()
structSetBody ty fields packed =
  withArrayLen fields
    (\len arr -> FFI.structSetBody ty arr (fromIntegral len) (fromBool packed))

-- | Get the number of elements defined inside the structure.
countStructElementTypes :: Num n => TypeRef -> n
countStructElementTypes = fromIntegral . FFI.countStructElementTypes

-- | Get the elements within a structure.
getStructElementTypes :: TypeRef -> IO [TypeRef]
getStructElementTypes struct =
  let
    len = countStructElementTypes struct
  in
    allocaArray len
      (\arr -> FFI.getStructElementTypes struct arr >> peekArray len arr)

-- | Determine whether a structure is packed.
isPackedStruct :: TypeRef -> Bool
isPackedStruct = toBool . FFI.isPackedStruct

-- | Determine whether a structure is opaque.
isOpaqueStruct :: TypeRef -> Bool
isOpaqueStruct = toBool . FFI.isOpaqueStruct

-- ** Array, pointer, and vector types

-- | Obtain the type of elements within a sequential type.
--
-- This works on array, vector, and pointer types.
getElementType :: TypeRef
               -- ^ Array type
               -> IO TypeRef
               -- ^ Element type
getElementType = FFI.getElementType

-- | Create a fixed size array type that refers to a specific type.
--
-- The created type will exist in the context that its element type
-- exists in.
arrayType :: Integral n => TypeRef
          -- ^ Element type
          -> n
          -- ^ Number of elements
          -> TypeRef
          -- ^ Array type
arrayType ty = FFI.arrayType ty . fromIntegral

-- | Obtain the length of an array type.
--
-- This only works on types that represent arrays.
getArrayLength :: Num n => TypeRef -> IO n
getArrayLength arr = FFI.getArrayLength arr >>= return . fromIntegral

-- | Create a pointer type that points to a defined type.
--
-- The created type will exist in the context that its pointee type
-- exists in.
pointerType :: Integral n => TypeRef
            -- ^ Pointed-to type
            -> n
            -- ^ Address space
            -> TypeRef
            -- ^ Pointer type
pointerType ty = FFI.pointerType ty . fromIntegral

-- | Obtain the address space of a pointer type.
--
-- This only works on types that represent pointers.
getPointerAddressSpace :: Num n => TypeRef -> IO n
getPointerAddressSpace arr =
  FFI.getPointerAddressSpace arr >>= return . fromIntegral

-- | Create a vector type that contains a defined type and has a specific
-- number of elements.
--
-- The created type will exist in the context thats its element type
-- exists in.
vectorType :: Integral n => TypeRef
           -- ^ Element type
           -> n
           -- ^ ELement count
           -> TypeRef
           -- ^ Vector type
vectorType ty = FFI.vectorType ty . fromIntegral

-- | Obtain the number of elements in a vector type.
--
-- This only works on types that represent vectors.
getVectorSize :: Num n => TypeRef -> IO n
getVectorSize arr = FFI.getVectorSize arr >>= return . fromIntegral

-- ** Other types

-- | Create a void type in a context.
voidTypeInContext :: ContextRef -> IO TypeRef
voidTypeInContext = FFI.voidTypeInContext

-- | Create a label type in a context.
labelTypeInContext :: ContextRef -> IO TypeRef
labelTypeInContext = FFI.labelTypeInContext

-- | Create a X86 MMX type in a context.
x86MMXTypeInContext :: ContextRef -> IO TypeRef
x86MMXTypeInContext = FFI.x86MMXTypeInContext

voidType :: TypeRef
voidType = FFI.voidType

labelType :: TypeRef
labelType = FFI.labelType

x86MMXType :: TypeRef
x86MMXType = FFI.x86MMXType

-- * Values

isAArgument :: ValueRef -> IO Bool
isAArgument = FFI.isAArgument

isABasicBlock :: ValueRef -> IO Bool
isABasicBlock = FFI.isABasicBlock

isAInlineAsm :: ValueRef -> IO Bool
isAInlineAsm = FFI.isAInlineAsm

isAMDNode :: ValueRef -> IO Bool
isAMDNode = FFI.isAMDNode

isAMDString :: ValueRef -> IO Bool
isAMDString = FFI.isAMDString

isAUser :: ValueRef -> IO Bool
isAUser = FFI.isAUser

isAConstant :: ValueRef -> IO Bool
isAConstant = FFI.isAConstant

isABlockAddress :: ValueRef -> IO Bool
isABlockAddress = FFI.isABlockAddress

isAConstantAggregateZero :: ValueRef -> IO Bool
isAConstantAggregateZero = FFI.isAConstantAggregateZero

isAConstantArray :: ValueRef -> IO Bool
isAConstantArray = FFI.isAConstantArray

isAConstantExpr :: ValueRef -> IO Bool
isAConstantExpr = FFI.isAConstantExpr

isAConstantFP :: ValueRef -> IO Bool
isAConstantFP = FFI.isAConstantFP

isAConstantInt :: ValueRef -> IO Bool
isAConstantInt = FFI.isAConstantInt

isAConstantPointerNull :: ValueRef -> IO Bool
isAConstantPointerNull = FFI.isAConstantPointerNull

isAConstantStruct :: ValueRef -> IO Bool
isAConstantStruct = FFI.isAConstantStruct

isAConstantVector :: ValueRef -> IO Bool
isAConstantVector = FFI.isAConstantVector

isAGlobalValue :: ValueRef -> IO Bool
isAGlobalValue = FFI.isAGlobalValue

isAFunction :: ValueRef -> IO Bool
isAFunction = FFI.isAFunction

isAGlobalAlias :: ValueRef -> IO Bool
isAGlobalAlias = FFI.isAGlobalAlias

isAGlobalVariable :: ValueRef -> IO Bool
isAGlobalVariable = FFI.isAGlobalVariable

isAUndefValue :: ValueRef -> IO Bool
isAUndefValue = FFI.isAUndefValue

isAInstruction :: ValueRef -> IO Bool
isAInstruction = FFI.isAInstruction

isABinaryOperator :: ValueRef -> IO Bool
isABinaryOperator = FFI.isABinaryOperator

isACallInst :: ValueRef -> IO Bool
isACallInst = FFI.isACallInst

isAIntrinsicInst :: ValueRef -> IO Bool
isAIntrinsicInst = FFI.isAIntrinsicInst

isADbgInfoIntrinsic :: ValueRef -> IO Bool
isADbgInfoIntrinsic = FFI.isADbgInfoIntrinsic

isADbgDeclareInst :: ValueRef -> IO Bool
isADbgDeclareInst = FFI.isADbgDeclareInst

isAMemIntrinsic :: ValueRef -> IO Bool
isAMemIntrinsic = FFI.isAMemIntrinsic

isAMemCpyInst :: ValueRef -> IO Bool
isAMemCpyInst = FFI.isAMemCpyInst

isAMemMoveInst :: ValueRef -> IO Bool
isAMemMoveInst = FFI.isAMemMoveInst

isAMemSetInst :: ValueRef -> IO Bool
isAMemSetInst = FFI.isAMemSetInst

isACmpInst :: ValueRef -> IO Bool
isACmpInst = FFI.isACmpInst

isAFCmpInst :: ValueRef -> IO Bool
isAFCmpInst = FFI.isAFCmpInst

isAICmpInst :: ValueRef -> IO Bool
isAICmpInst = FFI.isAICmpInst

isAExtractElementInst :: ValueRef -> IO Bool
isAExtractElementInst = FFI.isAExtractElementInst

isAGetElementPtrInst :: ValueRef -> IO Bool
isAGetElementPtrInst = FFI.isAGetElementPtrInst

isAInsertElementInst :: ValueRef -> IO Bool
isAInsertElementInst = FFI.isAInsertElementInst

isAInsertValueInst :: ValueRef -> IO Bool
isAInsertValueInst = FFI.isAInsertValueInst

isALandingPadInst :: ValueRef -> IO Bool
isALandingPadInst = FFI.isALandingPadInst

isAPHINode :: ValueRef -> IO Bool
isAPHINode = FFI.isAPHINode

isASelectInst :: ValueRef -> IO Bool
isASelectInst = FFI.isASelectInst

isAShuffleVectorInst :: ValueRef -> IO Bool
isAShuffleVectorInst = FFI.isAShuffleVectorInst

isAStoreInst :: ValueRef -> IO Bool
isAStoreInst = FFI.isAStoreInst

isATerminatorInst :: ValueRef -> IO Bool
isATerminatorInst = FFI.isATerminatorInst

isABranchInst :: ValueRef -> IO Bool
isABranchInst = FFI.isABranchInst

isAIndirectBrInst :: ValueRef -> IO Bool
isAIndirectBrInst = FFI.isAIndirectBrInst

isAInvokeInst :: ValueRef -> IO Bool
isAInvokeInst = FFI.isAInvokeInst

isAReturnInst :: ValueRef -> IO Bool
isAReturnInst = FFI.isAReturnInst

isASwitchInst :: ValueRef -> IO Bool
isASwitchInst = FFI.isASwitchInst

isAUnreachableInst :: ValueRef -> IO Bool
isAUnreachableInst = FFI.isAUnreachableInst

isAResumeInst :: ValueRef -> IO Bool
isAResumeInst = FFI.isAResumeInst

isAUnaryInstruction :: ValueRef -> IO Bool
isAUnaryInstruction = FFI.isAUnaryInstruction

isAAllocaInst :: ValueRef -> IO Bool
isAAllocaInst = FFI.isAAllocaInst

isACastInst :: ValueRef -> IO Bool
isACastInst = FFI.isACastInst

isABitCastInst :: ValueRef -> IO Bool
isABitCastInst = FFI.isABitCastInst

isAFPExtInst :: ValueRef -> IO Bool
isAFPExtInst = FFI.isAFPExtInst

isAFPToSIInst :: ValueRef -> IO Bool
isAFPToSIInst = FFI.isAFPToSIInst

isAFPToUIInst :: ValueRef -> IO Bool
isAFPToUIInst = FFI.isAFPToUIInst

isAFPTruncInst :: ValueRef -> IO Bool
isAFPTruncInst = FFI.isAFPTruncInst

isAIntToPtrInst :: ValueRef -> IO Bool
isAIntToPtrInst = FFI.isAIntToPtrInst

isAPtrToIntInst :: ValueRef -> IO Bool
isAPtrToIntInst = FFI.isAPtrToIntInst

isASExtInst :: ValueRef -> IO Bool
isASExtInst = FFI.isASExtInst

isASIToFPInst :: ValueRef -> IO Bool
isASIToFPInst = FFI.isASIToFPInst

isATruncInst :: ValueRef -> IO Bool
isATruncInst = FFI.isATruncInst

isAUIToFPInst :: ValueRef -> IO Bool
isAUIToFPInst = FFI.isAUIToFPInst

isAZExtInst :: ValueRef -> IO Bool
isAZExtInst = FFI.isAZExtInst

isAExtractValueInst :: ValueRef -> IO Bool
isAExtractValueInst = FFI.isAExtractValueInst

isALoadInst :: ValueRef -> IO Bool
isALoadInst = FFI.isALoadInst

isAVAArgInst :: ValueRef -> IO Bool
isAVAArgInst = FFI.isAVAArgInst

-- | Obtain the type of a value.
typeOf :: ValueRef -> IO TypeRef
typeOf = FFI.typeOf

-- | Obtain the string name of a value.
getValueName :: ValueRef -> IO String
getValueName val = FFI.getValueName val >>= peekCString

-- | Set the string name of a value.
setValueName :: ValueRef -> String -> IO ()
setValueName val str = withCString str (FFI.setValueName val)

-- | Dump a representation of a value to stderr.
dumpValue :: ValueRef -> IO ()
dumpValue = FFI.dumpValue

-- | Replace all uses of a value with another one.
replaceAllUsesWith :: ValueRef
                   -- ^ Old value
                   -> ValueRef
                   -- ^ New value
                   -> IO ()
replaceAllUsesWith = FFI.replaceAllUsesWith

-- | Determine whether the specified constant instance is constant.
isConstant :: ValueRef -> IO Bool
isConstant val = FFI.isConstant val >>= return . toBool

-- | Determine whether a value instance is undefined.
isUndef :: ValueRef -> IO Bool
isUndef val = FFI.isUndef val >>= return . toBool

isNull :: ValueRef -> IO Bool
isNull val = FFI.isNull val >>= return . toBool

-- ** Uses

-- | Obtain the first use of a value.
getFirstUse :: ValueRef -> IO UseRef
getFirstUse = FFI.getFirstUse

-- | Obtain the next use of a value.
--
-- This effectively advances the iterator. It returns @nullPtr@ if you are on
-- the final use and no more are available.
getNextUse :: UseRef -> IO UseRef
getNextUse = FFI.getNextUse

-- | Obtain the user value for a user.
getUser :: UseRef -> IO ValueRef
getUser = FFI.getUser

-- | Obtain the value this use corresponds to.
getUsedValue :: UseRef -> IO ValueRef
getUsedValue = FFI.getUsedValue

-- ** Users

-- | Obtain an operand at a specific index in a llvm::User value.
getOperand :: Integral n => ValueRef -> n -> IO ValueRef
getOperand val = FFI.getOperand val . fromIntegral

-- | Set an operand at a specific index in a user value.
setOperand :: Integral n => ValueRef -> n -> ValueRef -> IO ()
setOperand val ind mdata = FFI.setOperand val (fromIntegral ind) mdata

-- | Obtain the number of operands in a llvm::User value.
getNumOperands :: Integral n => ValueRef -> IO n
getNumOperands val = FFI.getNumOperands val >>= return . fromIntegral

-- ** Constants

-- | Obtain a constant value referring to the null instance of a type.
constNull :: TypeRef -> ValueRef
constNull = FFI.constNull

-- | Obtain a constant value referring to the instance of a type
-- consisting of all ones.
--
-- This is only valid for integer types.
constAllOnes :: TypeRef -> ValueRef
constAllOnes = FFI.constAllOnes

-- | Obtain a constant value referring to an undefined value of a
-- type.
getUndef :: TypeRef -> ValueRef
getUndef = FFI.getUndef

-- | Obtain a constant that is a constant pointer pointing to NULL for
-- a specified type.
constPointerNull :: TypeRef -> IO ValueRef
constPointerNull = FFI.constPointerNull

-- ** Scalar Constants

-- | Obtain a constant value for an integer type.
--
-- The returned value corresponds to a llvm::ConstantInt.
constInt :: Integral n => TypeRef
         -- ^ Type, must be an integer type
         -> n
         -- ^ Integer value
         -> Bool
         -- ^ Whether to sign extend
         -> ValueRef
         -- ^ Integer constant
constInt ty val sext = FFI.constInt ty (fromIntegral val) (fromBool sext)

-- | Obtain a constant value for an integer of arbitrary precision.
constIntOfArbitraryPrecision :: TypeRef
                             -- ^ Type, must be an integer type
                             -> [Word64]
                             -- ^ Integer data
                             -> ValueRef
                             -- ^ Integer constant
constIntOfArbitraryPrecision ty vals =
  unsafePerformIO
    (withArrayLen (map fromIntegral vals)
      (\len arr ->
        return (FFI.constIntOfArbitraryPrecision ty (fromIntegral len) arr)))

-- | Obtain a constant value for an integer parsed from a string.
constIntOfString :: Integral n => TypeRef
                 -- ^ Type, must be an integer type
                 -> String
                 -- ^ String literal
                 -> n
                 -- ^ Radix
                 -> IO ValueRef
                 -- ^ Integer constant
constIntOfString ty str radix =
  withCStringLen str
    (\(cstr, len) -> FFI.constIntOfStringAndSize ty cstr
                                                 (fromIntegral len)
                                                 (fromIntegral radix))

-- | Obtain a constant value referring to a double floating point value.
constReal :: Real n => TypeRef
          -- ^ Type, must be a floating point type
          -> n
          -- ^ Floating point value
          -> ValueRef
          -- ^ Floating point constant
constReal ty val = FFI.constReal ty (realToFrac val)

-- | Obtain a constant for a floating point value parsed from a string.
constRealOfString :: TypeRef
                  -- ^ Type, must be a floating point type
                  -> String
                  -- ^ String literal
                  -> IO ValueRef
                  -- ^ Floating point constant
constRealOfString ty str =
  withCStringLen str
    (\(cstr, len) -> FFI.constRealOfStringAndSize ty cstr (fromIntegral len))

-- | Obtain the zero extended value for an integer constant value.
constIntGetZExtValue :: Integral n => ValueRef -> IO n
constIntGetZExtValue val =
  FFI.constIntGetZExtValue val >>= return . fromIntegral

-- | Obtain the sign extended value for an integer constant value.
constIntGetSExtValue :: Integral n => ValueRef -> IO n
constIntGetSExtValue val =
  FFI.constIntGetSExtValue val >>= return . fromIntegral

-- ** Composite Constants

-- | Create a ConstantDataSequential and initialize it with a string.
constStringInContext :: ContextRef
                     -- ^ Context
                     -> String
                     -- ^ String constant
                     -> Bool
                     -- ^ Whether the string is null-terminated.
                     -> IO ValueRef
                     -- ^ String constant
constStringInContext ctx str nullterm =
  withCStringLen str
    (\(cstr, len) ->
      FFI.constStringInContext ctx cstr (fromIntegral len) (fromBool nullterm))

-- | Create a ConstantDataSequential and initialize it with a string
-- in the global context
constString :: String
            -- ^ String constant
            -> Bool
            -- ^ Whether the string is null-terminated.
            -> IO ValueRef
            -- ^ String constant
constString str nullterm =
  withCStringLen str
    (\(cstr, len) -> FFI.constString cstr (fromIntegral len) (fromBool nullterm))

-- | Create an anonymous ConstantStruct with the specified values.
constStructInContext :: ContextRef
                     -- ^ Context
                     -> [ValueRef]
                     -- ^ Elements
                     -> Bool
                     -- ^ Whether the structure is packed
                     -> IO ValueRef
                     -- ^ Constant value
constStructInContext ctx vals packed =
  withArrayLen vals
    (\len arr ->
      FFI.constStructInContext ctx arr (fromIntegral len) (fromBool packed))

-- | Create an anonymous ConstantStruct with the specified values in
-- the global context
constStruct :: [ValueRef]
            -- ^ Elements
            -> Bool
            -- ^ Whether the structure is packed
            -> ValueRef
            -- ^ Constant value
constStruct vals packed =
  unsafePerformIO
    (withArrayLen vals
      (\len arr ->
        return (FFI.constStruct arr (fromIntegral len) (fromBool packed))))

-- | Create a ConstantArray from values.
constArray :: TypeRef
           -- ^ Element type
           -> [ValueRef]
           -- ^ Element values
           -> ValueRef
           -- ^ Constant value
constArray ty vals =
  unsafePerformIO
    (withArrayLen vals
      (\len arr -> return (FFI.constArray ty arr (fromIntegral len))))

-- | Create a non-anonymous ConstantStruct from values.
constNamedStruct :: TypeRef
                 -- ^ Structure type
                 -> [ValueRef]
                 -- ^ Elements
                 -> IO ValueRef
                 -- ^ Constant value
constNamedStruct ty vals =
  withArrayLen vals
    (\len arr -> FFI.constNamedStruct ty arr (fromIntegral len))

-- | Create a ConstantVector from values.
constVector :: [ValueRef]
            -- ^ Elements
            -> ValueRef
            -- ^ Constant value
constVector vals =
  unsafePerformIO
    (withArrayLen vals
      (\len arr -> return (FFI.constVector arr (fromIntegral len))))

-- ** Constant Expressions

getConstOpcode :: ValueRef -> IO Opcode
getConstOpcode val = FFI.getConstOpcode val >>= return . FFI.toOpcode

alignOf :: TypeRef -> IO ValueRef
alignOf = FFI.alignOf

sizeOf :: TypeRef -> IO ValueRef
sizeOf = FFI.sizeOf

constNeg :: ValueRef -> ValueRef
constNeg = FFI.constNeg

constNSWNeg :: ValueRef -> IO ValueRef
constNSWNeg = FFI.constNSWNeg

constNUWNeg :: ValueRef -> IO ValueRef
constNUWNeg = FFI.constNUWNeg

constFNeg :: ValueRef -> ValueRef
constFNeg = FFI.constFNeg

constNot :: ValueRef -> ValueRef
constNot = FFI.constNot

constAdd :: ValueRef -> ValueRef -> ValueRef
constAdd = FFI.constAdd

constNSWAdd :: ValueRef -> ValueRef -> IO ValueRef
constNSWAdd = FFI.constNSWAdd

constNUWAdd :: ValueRef -> ValueRef -> IO ValueRef
constNUWAdd = FFI.constNUWAdd

constFAdd :: ValueRef -> ValueRef -> ValueRef
constFAdd = FFI.constFAdd

constSub :: ValueRef -> ValueRef -> ValueRef
constSub = FFI.constSub

constNSWSub :: ValueRef -> ValueRef -> IO ValueRef
constNSWSub = FFI.constNSWSub

constNUWSub :: ValueRef -> ValueRef -> IO ValueRef
constNUWSub = FFI.constNUWSub

constFSub :: ValueRef -> ValueRef -> ValueRef
constFSub = FFI.constFSub

constMul :: ValueRef -> ValueRef -> ValueRef
constMul = FFI.constMul

constNSWMul :: ValueRef -> ValueRef -> IO ValueRef
constNSWMul = FFI.constNSWMul

constNUWMul :: ValueRef -> ValueRef -> IO ValueRef
constNUWMul = FFI.constNUWMul

constFMul :: ValueRef -> ValueRef -> ValueRef
constFMul = FFI.constFMul

constUDiv :: ValueRef -> ValueRef -> ValueRef
constUDiv = FFI.constUDiv

constSDiv :: ValueRef -> ValueRef -> ValueRef
constSDiv = FFI.constSDiv

constExactSDiv :: ValueRef -> ValueRef -> IO ValueRef
constExactSDiv = FFI.constExactSDiv

constFDiv :: ValueRef -> ValueRef -> ValueRef
constFDiv = FFI.constFDiv

constURem :: ValueRef -> ValueRef -> ValueRef
constURem = FFI.constURem

constSRem :: ValueRef -> ValueRef -> ValueRef
constSRem = FFI.constSRem

constFRem :: ValueRef -> ValueRef -> ValueRef
constFRem = FFI.constFRem

constAnd :: ValueRef -> ValueRef -> ValueRef
constAnd = FFI.constAnd

constOr :: ValueRef -> ValueRef -> ValueRef
constOr = FFI.constOr

constXor :: ValueRef -> ValueRef -> ValueRef
constXor = FFI.constXor

constICmp :: IntPredicate -> ValueRef -> ValueRef -> ValueRef
constICmp pred = FFI.constICmp (FFI.fromIntPredicate pred)

constFCmp :: RealPredicate -> ValueRef -> ValueRef -> ValueRef
constFCmp pred = FFI.constFCmp (FFI.fromRealPredicate pred)

constShl :: ValueRef -> ValueRef -> ValueRef
constShl = FFI.constShl

constLShr :: ValueRef -> ValueRef -> ValueRef
constLShr = FFI.constLShr

constAShr :: ValueRef -> ValueRef -> ValueRef
constAShr = FFI.constAShr

constGEP :: ValueRef
         -- ^ Pointer value
         -> [ValueRef]
         -- ^ Indexes
         -> ValueRef
         -- ^ Constant value
constGEP val offs =
  unsafePerformIO
    (withArrayLen offs
      (\len arr -> return (FFI.constGEP val arr (fromIntegral len))))

constInBoundsGEP :: ValueRef
                 -- ^ Pointer value
                 -> [ValueRef]
                 -- ^ Indexes
                 -> IO ValueRef
                 -- ^ Constant value
constInBoundsGEP val offs =
  withArrayLen offs
    (\len arr -> FFI.constInBoundsGEP val arr (fromIntegral len))

constTrunc :: ValueRef -> TypeRef -> ValueRef
constTrunc = FFI.constTrunc

constSExt :: ValueRef -> TypeRef -> ValueRef
constSExt = FFI.constSExt

constZExt :: ValueRef -> TypeRef -> ValueRef
constZExt = FFI.constZExt

constFPTrunc :: ValueRef -> TypeRef -> ValueRef
constFPTrunc = FFI.constFPTrunc

constFPExt :: ValueRef -> TypeRef -> ValueRef
constFPExt = FFI.constFPExt

constUIToFP :: ValueRef -> TypeRef -> ValueRef
constUIToFP = FFI.constUIToFP

constSIToFP :: ValueRef -> TypeRef -> ValueRef
constSIToFP = FFI.constSIToFP

constFPToUI :: ValueRef -> TypeRef -> ValueRef
constFPToUI = FFI.constFPToUI

constFPToSI :: ValueRef -> TypeRef -> ValueRef
constFPToSI = FFI.constFPToSI

constPtrToInt :: ValueRef -> TypeRef -> ValueRef
constPtrToInt = FFI.constPtrToInt

constIntToPtr :: ValueRef -> TypeRef -> ValueRef
constIntToPtr = FFI.constIntToPtr

constBitCast :: ValueRef -> TypeRef -> ValueRef
constBitCast = FFI.constBitCast

constSExtOrBitCast :: ValueRef -> TypeRef -> IO ValueRef
constSExtOrBitCast = FFI.constSExtOrBitCast

constZExtOrBitCast :: ValueRef -> TypeRef -> IO ValueRef
constZExtOrBitCast = FFI.constZExtOrBitCast

constTruncOrBitCast :: ValueRef -> TypeRef -> IO ValueRef
constTruncOrBitCast = FFI.constTruncOrBitCast

constPointerCast :: ValueRef -> TypeRef -> IO ValueRef
constPointerCast = FFI.constPointerCast

constIntCast :: ValueRef
             -- ^ Value
             -> TypeRef
             -- ^ Result type, must be an integer
             -> Bool
             -- ^ Boolean indicating whether the value is signed
             -> IO ValueRef
             -- ^ Constant value
constIntCast val ty = FFI.constIntCast val ty . fromBool

constFPCast :: ValueRef -> TypeRef -> ValueRef
constFPCast = FFI.constFPCast

constSelect :: ValueRef
            -- ^ Test value
            -> ValueRef
            -- ^ True value
            -> ValueRef
            -- ^ False value
            -> ValueRef
            -- ^ Constant value
constSelect = FFI.constSelect

constExtractElement :: ValueRef
                    -- ^ Vector value
                    -> ValueRef
                    -- ^ Index
                    -> ValueRef
                    -- ^ Constant value
constExtractElement = FFI.constExtractElement

constInsertElement :: ValueRef
                   -- ^ Vector value
                   -> ValueRef
                   -- ^ Element value
                   -> ValueRef
                   -- ^ Index value
                   -> ValueRef
                   -- ^ Constant value
constInsertElement = FFI.constInsertElement

constShuffleVector :: ValueRef
                   -- ^ First vector
                   -> ValueRef
                   -- ^ Second vector
                   -> ValueRef
                   -- ^ Mask value
                   -> ValueRef
constShuffleVector = FFI.constShuffleVector

constExtractValue :: Integral n => ValueRef
                  -- ^ Aggregate constant
                  -> [n]
                  -- ^ Array of indexes
                  -> IO ValueRef
                  -- ^ Constant value
constExtractValue val offs =
  withArrayLen (map fromIntegral offs)
    (\len arr -> FFI.constExtractValue val arr (fromIntegral len))

constInsertValue :: Integral n => ValueRef
                 -- ^ Aggegate constant
                 -> ValueRef
                 -- ^ Element value
                 -> [n]
                 -- ^ Indexes
                 -> IO ValueRef
                 -- ^ Constant value
constInsertValue val elem offs =
  withArrayLen (map fromIntegral offs)
    (\len arr -> FFI.constInsertValue val elem arr (fromIntegral len))

constInlineAsm :: TypeRef
               -- ^ Result type
               -> String
               -- ^ Asm string
               -> String
               -- ^ Constraint string
               -> Bool
               -- ^ Whether or not the block has side effects.
               -> Bool
               -- ^ Whether or not the block preserves stack alignment.
               -> IO ValueRef
               -- ^ Constant value
constInlineAsm ty asm const effects align =
  withCString asm
    (\casm -> withCString const
      (\cconst ->
        FFI.constInlineAsm ty casm cconst (fromBool effects) (fromBool align)))

blockAddress :: ValueRef -> BasicBlockRef -> IO ValueRef
blockAddress = FFI.blockAddress

-- * Operations on globals

getGlobalParent :: ValueRef -> IO ModuleRef
getGlobalParent = FFI.getGlobalParent

isDeclaration :: ValueRef -> IO Bool
isDeclaration val = FFI.isDeclaration val >>= return . toBool

getLinkage :: ValueRef -> IO Linkage
getLinkage val = FFI.getLinkage val >>= return . FFI.toLinkage

setLinkage :: ValueRef -> Linkage -> IO ()
setLinkage val = FFI.setLinkage val . FFI.fromLinkage

getSection :: ValueRef -> IO String
getSection val = FFI.getSection val >>= peekCString

setSection :: ValueRef -> String -> IO ()
setSection val str = withCString str (FFI.setSection val)

getVisibility :: ValueRef -> IO Visibility
getVisibility val = FFI.getVisibility val >>= return . FFI.toVisibility

setVisibility :: ValueRef -> Visibility -> IO ()
setVisibility val = FFI.setVisibility val . FFI.fromVisibility

getAlignment :: Num n => ValueRef -> IO n
getAlignment val = FFI.getAlignment val >>= return . fromIntegral

setAlignment :: Integral n => ValueRef -> n -> IO ()
setAlignment val = FFI.setAlignment val . fromIntegral

-- * Global variables

addGlobal :: ModuleRef -> TypeRef -> String -> IO ValueRef
addGlobal mod ty str =
  withCString str (FFI.addGlobal mod ty)

addGlobalInAddressSpace :: Integral n => ModuleRef
                        -- ^ Module
                        -> TypeRef
                        -- ^ Type
                        -> String
                        -- ^ Name
                        -> n
                        -- ^ Address space
                        -> IO ValueRef
                        -- ^ Global variable
addGlobalInAddressSpace mod ty str spc =
  withCString str
    (\cstr -> FFI.addGlobalInAddressSpace mod ty cstr (fromIntegral spc))

getNamedGlobal :: ModuleRef -> String -> IO ValueRef
getNamedGlobal mod str =
  withCString str (FFI.getNamedGlobal mod)

getFirstGlobal :: ModuleRef -> IO ValueRef
getFirstGlobal = FFI.getFirstGlobal

getLastGlobal :: ModuleRef -> IO ValueRef
getLastGlobal = FFI.getLastGlobal

getNextGlobal :: ValueRef -> IO ValueRef
getNextGlobal = FFI.getNextGlobal

getPreviousGlobal :: ValueRef -> IO ValueRef
getPreviousGlobal = FFI.getPreviousGlobal

deleteGlobal :: ValueRef -> IO ()
deleteGlobal = FFI.deleteGlobal

setInitializer :: ValueRef -> ValueRef -> IO ()
setInitializer = FFI.setInitializer

getInitializer :: ValueRef -> IO ValueRef
getInitializer = FFI.getInitializer

isThreadLocal :: ValueRef -> IO Bool
isThreadLocal val = FFI.isThreadLocal val >>= return . toBool

setThreadLocal :: ValueRef -> Bool -> IO ()
setThreadLocal val = FFI.setThreadLocal val . fromBool

isGlobalConstant :: ValueRef -> IO Bool
isGlobalConstant val = FFI.isGlobalConstant val >>= return . toBool

setGlobalConstant :: ValueRef -> Bool -> IO ()
setGlobalConstant val = FFI.setGlobalConstant val . fromBool

-- Aliases

addAlias :: ModuleRef
         -- ^ Module
         -> TypeRef
         -- ^ Type
         -> ValueRef
         -- ^ Aliasee
         -> String
         -- ^ Name
         -> IO ValueRef
         -- ^ Alias value
addAlias mod ty val str = withCString str (FFI.addAlias mod ty val)

-- Functions

-- | Remove a function from its containing module and deletes it.
deleteFunction :: ValueRef -> IO ()
deleteFunction = FFI.deleteFunction

-- | Obtain the ID number from a function instance.
getIntrinsicID :: Integral n => ValueRef -> n
getIntrinsicID = fromIntegral . FFI.getIntrinsicID

-- | Obtain the calling function of a function.
getFunctionCallConv :: ValueRef -> IO CallingConvention
getFunctionCallConv val =
  FFI.getFunctionCallConv val >>= return . FFI.toCallingConvention

-- | Set the calling convention of a function.
setFunctionCallConv :: ValueRef -> CallingConvention -> IO ()
setFunctionCallConv val =
  FFI.setFunctionCallConv val . FFI.fromCallingConvention

-- | Obtain the name of the garbage collector to use during code
-- generation.
getGC :: ValueRef -> IO String
getGC val = FFI.getGC val >>= peekCString

-- | Define the garbage collector to use during code generation.
setGC :: ValueRef -> String -> IO ()
setGC val str = withCString str (FFI.setGC val)

-- | Add an attribute to a function.
addFunctionAttr :: ValueRef -> Attribute -> IO ()
addFunctionAttr val = FFI.addFunctionAttr val . FFI.fromAttribute

-- | Remove an attribute from a function.
removeFunctionAttr :: ValueRef -> Attribute -> IO ()
removeFunctionAttr val = FFI.removeFunctionAttr val . FFI.fromAttribute

-- Parameters

-- | Obtain the number of parameters in a function.
countParams :: Integral n => ValueRef -> n
countParams = fromIntegral . FFI.countParams

-- | Obtain the parameters in a function.
getParams :: ValueRef -> IO [ValueRef]
getParams func =
  let
    len = countParams func
  in
    allocaArray len (\arr -> FFI.getParams func arr >> peekArray len arr)

-- | Obtain the parameter at the specified index.
--
-- Parameters are indexed from 0.
getParam :: Integral n => ValueRef
         -- ^ Function value
         -> n
         -- ^ Parameter index
         -> ValueRef
         -- ^ Value
getParam val = FFI.getParam val . fromIntegral

-- | Obtain the function to which this argument belongs.
--
-- Unlike other functions in this group, this one takes a LLVMValueRef
-- that corresponds to a llvm::Attribute.
--
-- The returned LLVMValueRef is the llvm::Function to which this
-- argument belongs.
getParamParent :: ValueRef -> IO ValueRef
getParamParent = FFI.getParamParent

-- | Obtain the first parameter to a function.
getFirstParam :: ValueRef -> IO ValueRef
getFirstParam = FFI.getFirstParam

-- | Obtain the last parameter to a function.
getLastParam :: ValueRef -> IO ValueRef
getLastParam = FFI.getLastParam

-- | Obtain the next parameter to a function.
--
-- This takes a ValueRef obtained from getetFirstParam (which is
-- actually a wrapped iterator) and obtains the next parameter from the
-- underlying iterator.
getNextParam :: ValueRef -> IO ValueRef
getNextParam = FFI.getNextParam

-- | Obtain the previous parameter to a function.
--
-- This is the opposite of getNextParam.
getPreviousParam :: ValueRef -> IO ValueRef
getPreviousParam = FFI.getPreviousParam

-- | Add an attribute to a function argument.
addAttribute :: ValueRef -> Attribute -> IO ()
addAttribute val = FFI.addAttribute val . FFI.fromAttribute

-- | Remove an attribute from a function argument.
removeAttribute :: ValueRef -> Attribute -> IO ()
removeAttribute val = FFI.removeAttribute val . FFI.fromAttribute

-- | Set the alignment for a function parameter.
setParamAlignment :: Integral n => ValueRef -> n -> IO ()
setParamAlignment val = FFI.setParamAlignment val . fromIntegral

-- Metadata

-- | Obtain a MDString value from a context.
--
-- The returned instance corresponds to the llvm::MDString class.
--
-- The instance is specified by string data of a specified length. The
-- string content is copied, so the backing memory can be freed after
-- this function returns.
mdStringInContext :: ContextRef
                  -- ^ Context
                  -> String
                  -- ^ Metadata string
                  -> IO ValueRef
                  -- ^ Metadata node value
mdStringInContext ctx str =
  withCStringLen str
    (\(cstr, len) -> FFI.mdStringInContext ctx cstr (fromIntegral len))

-- | Obtain a MDString value from the global context.
mdString :: String
         -- ^ Metadata string
         -> IO ValueRef
         -- ^ Metadata node value
mdString str =
  withCStringLen str (\(cstr, len) -> FFI.mdString cstr (fromIntegral len))

-- | Obtain a MDNode value from a context.
--
-- The returned value corresponds to the llvm::MDNode class.
mdNodeInContext :: ContextRef
                -- ^ Context
                -> [ValueRef]
                -- ^ Values
                -> IO ValueRef
                -- ^ Metadata node value
mdNodeInContext ctx vals =
  withArrayLen vals (\len arr -> FFI.mdNodeInContext ctx arr (fromIntegral len))

-- | Obtain a MDNode value from the global context.
mdNode :: [ValueRef]
       -- ^ Values
       -> IO ValueRef
       -- ^ Metadata node value
mdNode vals =
  withArrayLen vals (\len arr -> FFI.mdNode arr (fromIntegral len))

readMDString :: ValueRef -> Ptr CUInt -> IO String
readMDString val ptr =
  do
    cstr <- FFI.getMDString val ptr
    len <- peek ptr
    peekCStringLen (cstr, fromIntegral len)

-- | Obtain the underlying string from a MDString value.
getMDString :: ValueRef -> IO String
getMDString val = alloca (readMDString val)

-- Basic blocks

-- | Convert a basic block instance to a value type.
basicBlockAsValue :: BasicBlockRef -> ValueRef
basicBlockAsValue = FFI.basicBlockAsValue

-- | Determine whether a LLVMValueRef is itself a basic block.
valueIsBasicBlock :: ValueRef -> Bool
valueIsBasicBlock = toBool . FFI.valueIsBasicBlock

-- | Convert a LLVMValueRef to a LLVMBasicBlockRef instance.
valueAsBasicBlock :: ValueRef -> BasicBlockRef
valueAsBasicBlock = FFI.valueAsBasicBlock

-- | Obtain the function to which a basic block belongs.
getBasicBlockParent :: BasicBlockRef -> IO ValueRef
getBasicBlockParent = FFI.getBasicBlockParent

-- | Obtain the terminator instruction for a basic block.
--
-- If the basic block does not have a terminator (it is not well-formed
-- if it doesn't), then NULL is returned.
--
-- The returned LLVMValueRef corresponds to a llvm::TerminatorInst.
getBasicBlockTerminator :: BasicBlockRef -> IO ValueRef
getBasicBlockTerminator = FFI.getBasicBlockTerminator

-- | Obtain the number of basic blocks in a function.
countBasicBlocks :: Integral n => ValueRef -> IO n
countBasicBlocks val = FFI.countBasicBlocks val >>= return . fromIntegral

-- | Obtain all of the basic blocks in a function.
getBasicBlocks :: ValueRef -> IO [BasicBlockRef]
getBasicBlocks func =
  do
    num <- countBasicBlocks func
    allocaArray num (\arr -> FFI.getBasicBlocks func arr >> peekArray num arr)

-- | Obtain the first basic block in a function.
--
-- The returned basic block can be used as an iterator. You will likely
-- eventually call into getNextBasicBlock with it.
getFirstBasicBlock :: ValueRef -> IO BasicBlockRef
getFirstBasicBlock = FFI.getFirstBasicBlock

-- | Obtain the last basic block in a function.
getLastBasicBlock :: ValueRef -> IO BasicBlockRef
getLastBasicBlock = FFI.getLastBasicBlock

-- | Advance a basic block iterator.
getNextBasicBlock :: BasicBlockRef -> IO BasicBlockRef
getNextBasicBlock = FFI.getNextBasicBlock

-- | Go backwards in a basic block iterator.
getPreviousBasicBlock :: BasicBlockRef -> IO BasicBlockRef
getPreviousBasicBlock = FFI.getPreviousBasicBlock

-- | Obtain the basic block that corresponds to the entry point of a
-- function.
getEntryBasicBlock :: ValueRef -> IO BasicBlockRef
getEntryBasicBlock = FFI.getEntryBasicBlock

-- | Append a basic block to the end of a function.
appendBasicBlockInContext :: ContextRef -> ValueRef -> String ->
                             IO BasicBlockRef
appendBasicBlockInContext ctx func str =
  withCString str (FFI.appendBasicBlockInContext ctx func)

-- | Insert a basic block in a function before another basic block.
--
-- The function to add to is determined by the function of the
-- passed basic block.
insertBasicBlockInContext :: ContextRef -> BasicBlockRef -> String ->
                             IO BasicBlockRef
insertBasicBlockInContext ctx func str =
  withCString str (FFI.insertBasicBlockInContext ctx func)

-- | Insert a basic block in a function using the global context.
appendBasicBlock :: ValueRef -> String -> IO BasicBlockRef
appendBasicBlock func str = withCString str (FFI.appendBasicBlock func)

-- | Insert a basic block in a function using the global context.
insertBasicBlock :: BasicBlockRef -> String -> IO BasicBlockRef
insertBasicBlock func str = withCString str (FFI.insertBasicBlock func)

-- | Remove a basic block from a function and delete it.
--
-- This deletes the basic block from its containing function and deletes
-- the basic block itself.
deleteBasicBlock :: BasicBlockRef -> IO ()
deleteBasicBlock = FFI.deleteBasicBlock

-- | Remove a basic block from a function.
--
-- This deletes the basic block from its containing function but keep
-- the basic block alive.
removeBasicBlockFromParent :: BasicBlockRef -> IO ()
removeBasicBlockFromParent = FFI.removeBasicBlockFromParent

-- | Move a basic block to before another one.
moveBasicBlockBefore :: BasicBlockRef
                     -- ^ Block
                     -> BasicBlockRef
                     -- ^ Position
                     -> IO ()
moveBasicBlockBefore = FFI.moveBasicBlockBefore

-- | Move a basic block to after another one.
moveBasicBlockAfter :: BasicBlockRef
                    -- ^ Block
                    -> BasicBlockRef
                    -- ^ Position
                    -> IO ()
moveBasicBlockAfter = FFI.moveBasicBlockAfter

-- | Obtain the first instruction in a basic block.
--
-- The returned LLVMValueRef corresponds to a llvm::Instruction
-- instance.
getFirstInstruction :: BasicBlockRef -> IO ValueRef
getFirstInstruction = FFI.getFirstInstruction

-- | Obtain the last instruction in a basic block.
--
-- The returned LLVMValueRef corresponds to a LLVM:Instruction.
getLastInstruction :: BasicBlockRef -> IO ValueRef
getLastInstruction = FFI.getLastInstruction

-- Instructions

-- | Determine whether an instruction has any metadata attached.
hasMetadata :: ValueRef -> IO Bool
hasMetadata val = FFI.hasMetadata val >>= return . toBool

-- | Return metadata associated with an instruction value.
getMetadata :: Integral n => ValueRef -> n -> IO ValueRef
getMetadata val = FFI.getMetadata val . fromIntegral

-- | Set metadata associated with an instruction value.
setMetadata :: Integral n => ValueRef -> n -> ValueRef -> IO ()
setMetadata val ind mdata = FFI.setMetadata val (fromIntegral ind) mdata

-- | Obtain the basic block to which an instruction belongs.
getInstructionParent :: ValueRef -> IO BasicBlockRef
getInstructionParent = FFI.getInstructionParent

-- | Obtain the instruction that occurs after the one specified.
--
-- The next instruction will be from the same basic block.
--
-- If this is the last instruction in a basic block, NULL will be
-- returned.
getNextInstruction :: ValueRef -> IO ValueRef
getNextInstruction = FFI.getNextInstruction

-- | Obtain the instruction that occured before this one.
--
-- If the instruction is the first instruction in a basic block, NULL
-- will be returned.
getPreviousInstruction :: ValueRef -> IO ValueRef
getPreviousInstruction = FFI.getPreviousInstruction

-- | Remove and delete an instruction.
--
-- The instruction specified is removed from its containing building
-- block and then deleted.
instructionEraseFromParent :: ValueRef -> IO ()
instructionEraseFromParent = FFI.instructionEraseFromParent

-- | Obtain the code opcode for an individual instruction.
getInstructionOpcode :: ValueRef -> IO Opcode
getInstructionOpcode val =
  FFI.getInstructionOpcode val >>= return . FFI.toOpcode

-- | Obtain the predicate of an instruction.
--
-- This is only valid for instructions that correspond to llvm::ICmpInst
-- or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
getICmpPredicate :: ValueRef -> IO IntPredicate
getICmpPredicate val = FFI.getICmpPredicate val >>= return . FFI.toIntPredicate

-- Call sites

-- | Set the calling convention for a call instruction.
--
-- This expects an LLVMValueRef that corresponds to a llvm::CallInst or
-- llvm::InvokeInst.
setInstructionCallConv :: ValueRef -> CallingConvention -> IO ()
setInstructionCallConv val =
  FFI.setInstructionCallConv val . FFI.fromCallingConvention 

-- | Obtain the calling convention for a call instruction.
getInstructionCallConv :: ValueRef -> IO CallingConvention
getInstructionCallConv val =
  FFI.getInstructionCallConv val >>= return . FFI.toCallingConvention

addInstrAttribute :: Integral n => ValueRef -> n -> Attribute -> IO ()
addInstrAttribute val n =
  FFI.addInstrAttribute val (fromIntegral n) . FFI.fromAttribute

removeInstrAttribute :: Integral n => ValueRef -> n -> Attribute -> IO ()
removeInstrAttribute val n =
  FFI.removeInstrAttribute val (fromIntegral n) . FFI.fromAttribute

setInstrParamAlignment :: (Integral n, Integral m) => ValueRef
                       -- ^ Instruction
                       -> m
                       -- ^ Argument index
                       -> n
                       -- ^ Alignment
                       -> IO ()
setInstrParamAlignment val ind =
  FFI.setInstrParamAlignment val (fromIntegral ind) . fromIntegral

-- Call instructions

-- | Obtain whether a call instruction is a tail call.
--
-- This only works on llvm::CallInst instructions.
isTailCall :: ValueRef -> IO Bool
isTailCall val = FFI.isTailCall val >>= return . toBool

-- | Set whether a call instruction is a tail call.
--
-- This only works on llvm::CallInst instructions.
setTailCall :: ValueRef -> Bool -> IO ()
setTailCall val = FFI.setTailCall val . fromBool

-- Switch instruction

-- | Obtain the default destination basic block of a switch instruction.
--
-- This only works on llvm::SwitchInst instructions.
getSwitchDefaultDest :: ValueRef -> IO BasicBlockRef
getSwitchDefaultDest = FFI.getSwitchDefaultDest

-- Phi instructions

-- | Add incoming values to the end of a PHI list.
addIncoming :: ValueRef
            -- ^ Phi instruction
            -> [(ValueRef, ValueRef)]
            -- ^ List of (value, basic block) pairs
            -> IO ()
addIncoming val ins =
  let
    (vals, blocks) = unzip ins
  in
    withArray vals
      (\varr -> withArrayLen blocks
        (\len barr -> FFI.addIncoming val barr varr (fromIntegral len)))

-- | Obtain the number of incoming basic blocks to a PHI node.
countIncoming :: Integral n => ValueRef -> IO n
countIncoming val = FFI.countIncoming val >>= return . fromIntegral

-- | Obtain an incoming value to a PHI node as a LLVMValueRef.
getIncomingValue :: Integral n => ValueRef -> n -> IO ValueRef
getIncomingValue val = FFI.getIncomingValue val . fromIntegral

-- | Obtain an incoming value to a PHI node as a LLVMBasicBlockRef.
getIncomingBlock :: Integral n => ValueRef -> n -> IO BasicBlockRef
getIncomingBlock val = FFI.getIncomingBlock val . fromIntegral

-- Builders

createBuilderInContext :: ContextRef -> IO BuilderRef
createBuilderInContext = FFI.createBuilderInContext

createBuilder :: IO BuilderRef
createBuilder = FFI.createBuilder

positionBuilder :: BuilderRef -> BasicBlockRef -> ValueRef -> IO ()
positionBuilder = FFI.positionBuilder

positionBefore :: BuilderRef -> ValueRef -> IO ()
positionBefore = FFI.positionBefore

positionAtEnd :: BuilderRef -> BasicBlockRef -> IO ()
positionAtEnd = FFI.positionAtEnd

getInsertBlock :: BuilderRef -> IO BasicBlockRef
getInsertBlock = FFI.getInsertBlock

clearInsertionPosition :: BuilderRef -> IO ()
clearInsertionPosition = FFI.clearInsertionPosition

insertIntoBuilder :: BuilderRef -> ValueRef -> IO ()
insertIntoBuilder = FFI.insertIntoBuilder

insertIntoBuilderWithName :: BuilderRef -> ValueRef -> String -> IO ()
insertIntoBuilderWithName b val str =
  withCString str (FFI.insertIntoBuilderWithName b val)

disposeBuilder :: BuilderRef -> IO ()
disposeBuilder = FFI.disposeBuilder

-- Metadata

getCurrentDebugLocation :: BuilderRef -> IO ValueRef
getCurrentDebugLocation = FFI.getCurrentDebugLocation

setCurrentDebugLocation :: BuilderRef -> ValueRef -> IO ()
setCurrentDebugLocation = FFI.setCurrentDebugLocation

setInstDebugLocation :: BuilderRef -> ValueRef -> IO ()
setInstDebugLocation = FFI.setInstDebugLocation

-- Terminators

buildRetVoid :: BuilderRef -> IO ValueRef
buildRetVoid = FFI.buildRetVoid

buildRet :: BuilderRef -> ValueRef -> IO ValueRef
buildRet = FFI.buildRet

buildAggregateRet :: BuilderRef -> [ValueRef] -> IO ValueRef
buildAggregateRet b vals =
  withArrayLen vals (\len arr -> FFI.buildAggregateRet b arr (fromIntegral len))

buildBr :: BuilderRef -> BasicBlockRef -> IO ValueRef
buildBr = FFI.buildBr

buildCondBr :: BuilderRef -> ValueRef -> BasicBlockRef -> BasicBlockRef ->
               IO ValueRef
buildCondBr = FFI.buildCondBr

buildSwitch :: Integral n =>
               BuilderRef -> ValueRef -> BasicBlockRef -> n -> IO ValueRef
buildSwitch b val block = FFI.buildSwitch b val block . fromIntegral

buildIndirectBr :: Integral n => BuilderRef -> ValueRef -> n -> IO ValueRef
buildIndirectBr b val = FFI.buildIndirectBr b val . fromIntegral

buildInvoke :: BuilderRef -> ValueRef -> [ValueRef] -> BasicBlockRef ->
               BasicBlockRef -> String -> IO ValueRef
buildInvoke b func args cont catch str =
  withCString str
    (\cstr -> withArrayLen args
      (\len arr ->
        FFI.buildInvoke b func arr (fromIntegral len) cont catch cstr))

buildLandingPad :: Integral n =>
                   BuilderRef -> TypeRef -> ValueRef -> n -> String ->
                   IO ValueRef
buildLandingPad b ty val count str =
  withCString str (FFI.buildLandingPad b ty val (fromIntegral count))

buildResume :: BuilderRef -> ValueRef -> IO ValueRef
buildResume = FFI.buildResume

buildUnreachable :: BuilderRef -> IO ValueRef
buildUnreachable = FFI.buildUnreachable

-- Switch instructions

addCase :: ValueRef -> ValueRef -> BasicBlockRef -> IO ()
addCase = FFI.addCase

-- IndirectBr instructions

addDestination :: ValueRef -> BasicBlockRef -> IO ()
addDestination = FFI.addDestination

-- LandingPad instructions

addClause :: ValueRef -> ValueRef -> IO ()
addClause = FFI.addClause

-- Resume instructions

setCleanup :: ValueRef -> Bool -> IO ()
setCleanup val = FFI.setCleanup val . fromBool

-- Arithmetic

buildAdd :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildAdd b l r str = withCString str (FFI.buildAdd b l r)

buildNSWAdd :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildNSWAdd b l r str = withCString str (FFI.buildNSWAdd b l r)

buildNUWAdd :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildNUWAdd b l r str = withCString str (FFI.buildNUWAdd b l r)

buildFAdd :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildFAdd b l r str = withCString str (FFI.buildFAdd b l r)

buildSub :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildSub b l r str = withCString str (FFI.buildSub b l r)

buildNSWSub :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildNSWSub b l r str = withCString str (FFI.buildNSWSub b l r)

buildNUWSub :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildNUWSub b l r str = withCString str (FFI.buildNUWSub b l r)

buildFSub :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildFSub b l r str = withCString str (FFI.buildFSub b l r)

buildMul :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildMul b l r str = withCString str (FFI.buildMul b l r)

buildNSWMul :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildNSWMul b l r str = withCString str (FFI.buildNSWMul b l r)

buildNUWMul :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildNUWMul b l r str = withCString str (FFI.buildNUWMul b l r)

buildFMul :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildFMul b l r str = withCString str (FFI.buildFMul b l r)

buildUDiv :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildUDiv b l r str = withCString str (FFI.buildUDiv b l r)

buildSDiv :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildSDiv b l r str = withCString str (FFI.buildSDiv b l r)

buildExactSDiv :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildExactSDiv b l r str = withCString str (FFI.buildExactSDiv b l r)

buildFDiv :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildFDiv b l r str = withCString str (FFI.buildFDiv b l r)

buildURem :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildURem b l r str = withCString str (FFI.buildURem b l r)

buildSRem :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildSRem b l r str = withCString str (FFI.buildSRem b l r)

buildFRem :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildFRem b l r str = withCString str (FFI.buildFRem b l r)

buildShl :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildShl b l r str = withCString str (FFI.buildShl b l r)

buildAShr :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildAShr b l r str = withCString str (FFI.buildAShr b l r)

buildLShr :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildLShr b l r str = withCString str (FFI.buildLShr b l r)

buildAnd :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildAnd b l r str = withCString str (FFI.buildAnd b l r)

buildOr :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildOr b l r str = withCString str (FFI.buildOr b l r)

buildXor :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildXor b l r str = withCString str (FFI.buildXor b l r)

buildBinOp :: BuilderRef -> Opcode -> ValueRef -> ValueRef -> String ->
              IO ValueRef
buildBinOp b op l r str =
  withCString str (FFI.buildBinOp b (FFI.fromOpcode op) l r)

buildNeg :: BuilderRef -> ValueRef -> String -> IO ValueRef
buildNeg b val str = withCString str (FFI.buildNeg b val)

buildNSWNeg :: BuilderRef -> ValueRef -> String -> IO ValueRef
buildNSWNeg b val str = withCString str (FFI.buildNSWNeg b val)

buildNUWNeg :: BuilderRef -> ValueRef -> String -> IO ValueRef
buildNUWNeg b val str = withCString str (FFI.buildNUWNeg b val)

buildFNeg :: BuilderRef -> ValueRef -> String -> IO ValueRef
buildFNeg b val str = withCString str (FFI.buildFNeg b val)

buildNot :: BuilderRef -> ValueRef -> String -> IO ValueRef
buildNot b val str = withCString str (FFI.buildNot b val)

-- Memory

buildMalloc :: BuilderRef -> TypeRef -> String -> IO ValueRef
buildMalloc b ty str = withCString str (FFI.buildMalloc b ty)

buildArrayMalloc :: BuilderRef -> TypeRef -> ValueRef -> String -> IO ValueRef
buildArrayMalloc b ty val str = withCString str (FFI.buildArrayMalloc b ty val)

buildAlloca :: BuilderRef -> TypeRef -> String -> IO ValueRef
buildAlloca b ty str = withCString str (FFI.buildAlloca b ty)

buildArrayAlloca :: BuilderRef -> TypeRef -> ValueRef -> String -> IO ValueRef
buildArrayAlloca b ty val str = withCString str (FFI.buildArrayAlloca b ty val)

buildFree :: BuilderRef -> ValueRef -> IO ValueRef
buildFree = FFI.buildFree

buildLoad :: BuilderRef -> ValueRef -> String -> IO ValueRef
buildLoad b val str = withCString str (FFI.buildLoad b val)

buildStore :: BuilderRef
           -- ^ Builder
           -> ValueRef
           -- ^ Value
           -> ValueRef
           -- ^ Pointer value
           -> IO ValueRef
           -- ^ Store instruction
buildStore = FFI.buildStore

buildGEP :: BuilderRef
         -- ^ Builder
         -> ValueRef
         -- ^ Pointer value
         -> [ValueRef]
         -- ^ Indexes
         -> String
         -- ^ Name
         -> IO ValueRef
buildGEP b val inds str =
  withArrayLen inds
    (\len arr -> withCString str
      (\cstr -> FFI.buildGEP b val arr (fromIntegral len) cstr))

buildInBoundsGEP :: BuilderRef
                 -- ^ Builder
                 -> ValueRef
                 -- ^ Pointer value
                 -> [ValueRef]
                 -- ^ Indexes
                 -> String
                 -- ^ Name
                 -> IO ValueRef
buildInBoundsGEP b val inds str =
  withArrayLen inds
    (\len arr -> withCString str
      (\cstr -> FFI.buildInBoundsGEP b val arr (fromIntegral len) cstr))

buildStructGEP :: Integral i =>
                  BuilderRef -> ValueRef -> i -> String -> IO ValueRef
buildStructGEP b val ind str =
  withCString str (FFI.buildStructGEP b val (fromIntegral ind))

buildGlobalString :: BuilderRef -> String -> String -> IO ValueRef
buildGlobalString b name str =
  withCString name
    (\cname -> withCString str
      (\cstr -> FFI.buildGlobalString b cname cstr))

buildGlobalStringPtr :: BuilderRef -> String -> String -> IO ValueRef
buildGlobalStringPtr b name str =
  withCString name
    (\cname -> withCString str
      (\cstr -> FFI.buildGlobalStringPtr b cname cstr))

getVolatile :: ValueRef -> IO Bool
getVolatile val = FFI.getVolatile val >>= return . toBool

setVolatile :: ValueRef -> Bool -> IO ()
setVolatile val = FFI.setVolatile val . fromBool

-- Casts

buildTrunc :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildTrunc b val ty str = withCString str (FFI.buildTrunc b val ty)

buildZExt :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildZExt b val ty str = withCString str (FFI.buildZExt b val ty)

buildSExt :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildSExt b val ty str = withCString str (FFI.buildSExt b val ty)

buildFPToUI :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildFPToUI b val ty str = withCString str (FFI.buildFPToUI b val ty)

buildFPToSI :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildFPToSI b val ty str = withCString str (FFI.buildFPToSI b val ty)

buildUIToFP :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildUIToFP b val ty str = withCString str (FFI.buildUIToFP b val ty)

buildSIToFP :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildSIToFP b val ty str = withCString str (FFI.buildSIToFP b val ty)

buildFPTrunc :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildFPTrunc b val ty str = withCString str (FFI.buildFPTrunc b val ty)

buildFPExt :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildFPExt b val ty str = withCString str (FFI.buildFPExt b val ty)

buildPtrToInt :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildPtrToInt b val ty str = withCString str (FFI.buildIntToPtr b val ty)

buildIntToPtr :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildIntToPtr b val ty str = withCString str (FFI.buildIntToPtr b val ty)

buildBitCast :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildBitCast b val ty str = withCString str (FFI.buildBitCast b val ty)

buildZExtOrBitCast :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildZExtOrBitCast b val ty str =
  withCString str (FFI.buildZExtOrBitCast b val ty)

buildSExtOrBitCast :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildSExtOrBitCast b val ty str =
  withCString str (FFI.buildSExtOrBitCast b val ty)

buildTruncOrBitCast :: BuilderRef -> ValueRef -> TypeRef -> String ->
                       IO ValueRef
buildTruncOrBitCast b val ty str =
  withCString str (FFI.buildTruncOrBitCast b val ty)

buildCast :: BuilderRef -> Opcode -> ValueRef -> TypeRef -> String ->
             IO ValueRef
buildCast b op val ty str =
  withCString str (FFI.buildCast b (FFI.fromOpcode op) val ty)

buildPointerCast :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildPointerCast b val ty str = withCString str (FFI.buildPointerCast b val ty)

buildIntCast :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildIntCast b val ty str = withCString str (FFI.buildIntCast b val ty)

buildFPCast :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildFPCast b val ty str = withCString str (FFI.buildFPCast b val ty)

-- Comparisons

buildICmp :: BuilderRef -> IntPredicate -> ValueRef -> ValueRef -> String ->
             IO ValueRef
buildICmp b pred l r str =
  withCString str (FFI.buildICmp b (FFI.fromIntPredicate pred) l r)

buildFCmp :: BuilderRef -> RealPredicate -> ValueRef -> ValueRef -> String ->
             IO ValueRef
buildFCmp b pred l r str =
  withCString str (FFI.buildFCmp b (FFI.fromRealPredicate pred) l r)

-- Miscellaneous

buildPhi :: BuilderRef -> TypeRef -> String -> IO ValueRef
buildPhi b ty str = withCString str (FFI.buildPhi b ty)

buildCall :: BuilderRef -> ValueRef -> [ValueRef] -> String -> IO ValueRef
buildCall b val args str =
  withCString str
    (\cstr -> withArrayLen args
      (\len arr -> FFI.buildCall b val arr (fromIntegral len) cstr))

buildSelect :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> String ->
               IO ValueRef
buildSelect b test tval fval str =
  withCString str (FFI.buildSelect b test tval fval)

buildVAArg :: BuilderRef -> ValueRef -> TypeRef -> String -> IO ValueRef
buildVAArg b list ty str =
  withCString str (FFI.buildVAArg b list ty)

buildExtractElement :: BuilderRef -> ValueRef -> ValueRef -> String ->
                       IO ValueRef
buildExtractElement b vec ind str =
  withCString str (FFI.buildExtractElement b vec ind)

buildInsertElement :: BuilderRef -> ValueRef -> ValueRef -> ValueRef ->
                      String -> IO ValueRef
buildInsertElement b vec ind val str =
  withCString str (FFI.buildInsertElement b vec ind val)

buildShuffleVector :: BuilderRef -> ValueRef -> ValueRef -> ValueRef ->
                      String -> IO ValueRef
buildShuffleVector b vec ind mask str =
  withCString str (FFI.buildInsertElement b vec ind mask)

buildExtractValue :: Integral n =>
                     BuilderRef -> ValueRef -> n -> String -> IO ValueRef
buildExtractValue b vec ind str =
  withCString str (FFI.buildExtractValue b vec (fromIntegral ind))

buildInsertValue :: Integral n =>
                    BuilderRef -> ValueRef -> ValueRef -> n -> String ->
                    IO ValueRef
buildInsertValue b vec val ind str =
  withCString str (FFI.buildInsertValue b vec val (fromIntegral ind))

buildIsNull :: BuilderRef -> ValueRef -> String -> IO ValueRef
buildIsNull b val str = withCString str (FFI.buildIsNull b val)

buildIsNotNull :: BuilderRef -> ValueRef -> String -> IO ValueRef
buildIsNotNull b val str = withCString str (FFI.buildIsNotNull b val)

buildPtrDiff :: BuilderRef -> ValueRef -> ValueRef -> String -> IO ValueRef
buildPtrDiff b l r str = withCString str (FFI.buildPtrDiff b l r)

-- Module providers
-- | Changes the type of M so it can be passed to FunctionPassManagers and the
-- JIT.  They take ModuleProviders for historical reasons.
createModuleProviderForExistingModule :: ModuleRef -> IO ModuleProviderRef
createModuleProviderForExistingModule =
  FFI.createModuleProviderForExistingModule

-- | Destroys the module M.
disposeModuleProvider :: ModuleProviderRef -> IO ()
disposeModuleProvider = FFI.disposeModuleProvider

-- Memory buffers

createMemoryBufferWithContentsOfFile :: String ->
                                        IO (Either MemoryBufferRef String)
createMemoryBufferWithContentsOfFile str =
  withCString str
    (\cstr -> alloca
      (\mbufref -> alloca
        (\msgref ->
          do
            res <- FFI.createMemoryBufferWithContentsOfFile cstr mbufref msgref
            if 1 == res
              then do
                out <- peek mbufref
                return (Left out)
              else do
                msg <- peek msgref
                out <- peekCString msg
                FFI.disposeMessage msg
                return (Right out))))

createMemoryBufferWithSTDIN :: IO (Either MemoryBufferRef String)
createMemoryBufferWithSTDIN =
  alloca
    (\mbufref -> alloca
      (\msgref ->
        do
          res <- FFI.createMemoryBufferWithSTDIN mbufref msgref
          if 1 == res
            then do
              out <- peek mbufref
              return (Left out)
            else do
              msg <- peek msgref
              out <- peekCString msg
              FFI.disposeMessage msg
              return (Right out)))

disposeMemoryBuffer :: MemoryBufferRef -> IO ()
disposeMemoryBuffer = FFI.disposeMemoryBuffer

-- Pass Registry
-- | Return the global pass registry, for use with initialization functions.
getGlobalPassRegistry :: IO PassRegistryRef
getGlobalPassRegistry = FFI.getGlobalPassRegistry

-- Pass Manager

-- | Constructs a new whole-module pass pipeline. This type of
-- pipeline is suitable for link-time optimization and whole-module
-- transformations.
createPassManager :: IO PassManagerRef
createPassManager = FFI.createPassManager

-- | Constructs a new function-by-function pass pipeline over the
-- module provider. It does not take ownership of the module
-- provider. This type of pipeline is suitable for code generation and
-- JIT compilation tasks.
createFunctionPassManagerForModule :: ModuleRef -> IO PassManagerRef
createFunctionPassManagerForModule = FFI.createFunctionPassManagerForModule

-- | Deprecated: Use LLVMCreateFunctionPassManagerForModule instead.
createFunctionPassManager :: ModuleProviderRef -> IO PassManagerRef
createFunctionPassManager = FFI.createFunctionPassManager

-- | Initializes, executes on the provided module, and finalizes all
-- of the passes scheduled in the pass manager. Returns 1 if any of
-- the passes modified the module, 0 otherwise.
runPassManager :: PassManagerRef -> ModuleRef -> IO Bool
runPassManager mgr mod = FFI.runPassManager mgr mod >>= return . toBool

-- | Initializes all of the function passes scheduled in the function
-- pass manager. Returns 1 if any of the passes modified the module, 0
-- otherwise.
initializeFunctionPassManager :: PassManagerRef -> IO Bool
initializeFunctionPassManager mgr =
  FFI.initializeFunctionPassManager mgr >>= return . toBool

-- | Executes all of the function passes scheduled in the function
-- pass manager on the provided function. Returns 1 if any of the
-- passes modified the function, false otherwise.
runFunctionPassManager :: PassManagerRef -> ValueRef -> IO Bool
runFunctionPassManager mgr val =
  FFI.runFunctionPassManager mgr val >>= return . toBool

-- | Finalizes all of the function passes scheduled in in the function
-- pass manager. Returns 1 if any of the passes modified the module, 0
-- otherwise.
finalizeFunctionPassManager :: PassManagerRef -> IO Bool
finalizeFunctionPassManager mgr =
  FFI.finalizeFunctionPassManager mgr >>= return . toBool

-- | Frees the memory of a pass pipeline. For function pipelines, does
-- not free the module provider.
disposePassManager :: PassManagerRef -> IO ()
disposePassManager = FFI.disposePassManager
