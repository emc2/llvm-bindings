{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

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

-- | Raw FFI bindings for llvm-c/Core.h.  This is provided only for
-- utility purposes; it is advisable to use LLVM.Core instead.
module LLVM.FFI.Core(
       -- * Types
       ModuleRef,
       ContextRef,
       TypeRef,
       UseRef,
       ValueRef,
       BasicBlockRef,
       BuilderRef,
       ModuleProviderRef,
       MemoryBufferRef,
       PassManagerRef,
       PassRegistryRef,
       Opcode(..),
       Attribute(..),
       TypeKind(..),
       Linkage(..),
       Visibility(..),
       IntPredicate(..),
       RealPredicate(..),
       CallingConvention(..),

       -- * Conversion between C and Haskell
       fromOpcode,
       toOpcode,
       toTypeKind,
       fromTypeKind,
       fromAttribute,
       toAttribute,
       fromLinkage,
       toLinkage,
       fromVisibility,
       toVisibility,
       toIntPredicate,
       fromIntPredicate,
       toRealPredicate,
       fromRealPredicate,
       fromCallingConvention,
       toCallingConvention,

       -- * Error handling
       disposeMessage,

       -- * Context functions
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
       ptrDisposeModule,

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
       halfTypeInContext,
       floatTypeInContext,
       doubleTypeInContext,
       x86FP80TypeInContext,
       fp128TypeInContext,
       ppcFP128TypeInContext,
       halfType,
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
       constIntOfStringAndSize,
       constReal,
       constRealOfString,
       constRealOfStringAndSize,
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
       getFunctionAttr,
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
       getAttribute,
       setParamAlignment,

       -- * Metadata
       mdStringInContext,
       mdString,
       mdNodeInContext,
       mdNode,
       getMDString,

       -- * Basic blocks
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

       -- * Instructions
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
       ptrDisposeBuilder,

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

       -- * Module Providers
       createModuleProviderForExistingModule,
       disposeModuleProvider,

       -- * Memory buffers
       createMemoryBufferWithContentsOfFile,
       createMemoryBufferWithSTDIN,
       disposeMemoryBuffer,

       -- * PassRegistry
       getGlobalPassRegistry,

       -- * Pass manager
       ptrDisposePassManager,

       createPassManager,
       createFunctionPassManagerForModule,
       createFunctionPassManager,
       runPassManager,
       initializeFunctionPassManager,
       runFunctionPassManager,
       finalizeFunctionPassManager,
       disposePassManager
       ) where

import Data.Typeable hiding (typeOf)
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#include <llvm-c/Core.h>

data Context
    deriving (Typeable)

-- | The top-level container for all LLVM global data. See the
-- LLVMContext class.
type ContextRef = Ptr Context

data Module
    deriving (Typeable)

-- | The top-level container for all other LLVM Intermediate
-- Representation (IR) objects.  See llvm::Module.
type ModuleRef = Ptr Module

data ModuleProvider
    deriving (Typeable)

-- | Interface used to provide a module to JIT or interpreter.  This
-- is now just a synonym for llvm::Module, but we have to keep using
-- the different type to keep binary compatibility.
type ModuleProviderRef = Ptr ModuleProvider

data Type
    deriving (Typeable)

-- | Each value in the LLVM IR has a type, an LLVMTypeRef.  See
-- llvm::Type.
type TypeRef = Ptr Type

type BasicBlock = Value

-- | Represents a basic block of instruction in LLVM IR.  See
-- llvm::BasicBlock.
type BasicBlockRef = Ptr BasicBlock

data Value
    deriving (Typeable)

-- | Represents an individual value in LLVM IR.  See llvm::Value.
type ValueRef = Ptr Value

data OpaqueUse
    deriving (Typeable)

-- | Used to get the users and usees of a Value.  See llvm::Use.
type UseRef = Ptr OpaqueUse

data Builder
    deriving (Typeable)

-- | Represents an LLVM basic block builder. See llvm::IRBuilder.
type BuilderRef = Ptr Builder

data MemoryBuffer
    deriving (Typeable)

-- | Used to provide a module to JIT or interpreter.  See
-- llvm::MemoryBuffer.
type MemoryBufferRef = Ptr MemoryBuffer

data PassManager
    deriving (Typeable)
type PassManagerRef = Ptr PassManager

data PassRegistry
    deriving (Typeable)
type PassRegistryRef = Ptr PassRegistry

data Opcode =
    Ret
  | Br
  | Switch
  | IndirectBr
  | Invoke
  | Unreachable
  | Add
  | FAdd
  | Sub
  | FSub
  | Mul
  | FMul
  | UDiv
  | SDiv
  | FDiv
  | URem
  | SRem
  | FRem
  | Shl
  | LShr
  | AShr
  | And
  | Or
  | Xor
  | Alloca
  | Load
  | Store
  | GetElementPtr
  | Trunc
  | ZExt
  | SExt
  | FPToUI
  | FPToSI
  | UIToFP
  | SIToFP
  | FPTrunc
  | FPExt
  | PtrToInt
  | IntToPtr
  | BitCast
  | ICmp
  | FCmp
  | PHI
  | Call
  | Select
  | VAArg
  | ExtractElement
  | InsertElement
  | ShuffleVector
  | ExtractValue
  | InsertValue
  | Fence
  | AtomicCmpXchg
  | AtomicRMW
  | Resume
  | LandingPad

fromOpcode :: Opcode -> COpcode
fromOpcode Ret = (#const LLVMRet)
fromOpcode Br = (#const LLVMBr)
fromOpcode Switch = (#const LLVMSwitch)
fromOpcode IndirectBr = (#const LLVMIndirectBr)
fromOpcode Invoke = (#const LLVMInvoke)
fromOpcode Unreachable = (#const LLVMUnreachable)
fromOpcode Add = (#const LLVMAdd)
fromOpcode FAdd = (#const LLVMFAdd)
fromOpcode Sub = (#const LLVMSub)
fromOpcode FSub = (#const LLVMFSub)
fromOpcode Mul = (#const LLVMMul)
fromOpcode FMul = (#const LLVMFMul)
fromOpcode UDiv = (#const LLVMUDiv)
fromOpcode SDiv = (#const LLVMSDiv)
fromOpcode FDiv = (#const LLVMFDiv)
fromOpcode URem = (#const LLVMURem)
fromOpcode SRem = (#const LLVMSRem)
fromOpcode FRem = (#const LLVMFRem)
fromOpcode Shl = (#const LLVMShl)
fromOpcode LShr = (#const LLVMLShr)
fromOpcode AShr = (#const LLVMAShr)
fromOpcode And = (#const LLVMAnd)
fromOpcode Or = (#const LLVMOr)
fromOpcode Xor = (#const LLVMXor)
fromOpcode Alloca = (#const LLVMAlloca)
fromOpcode Load = (#const LLVMLoad)
fromOpcode Store = (#const LLVMStore)
fromOpcode GetElementPtr = (#const LLVMGetElementPtr)
fromOpcode Trunc = (#const LLVMTrunc)
fromOpcode ZExt = (#const LLVMZExt)
fromOpcode SExt = (#const LLVMSExt)
fromOpcode FPToUI = (#const LLVMFPToUI)
fromOpcode FPToSI = (#const LLVMFPToSI)
fromOpcode UIToFP = (#const LLVMUIToFP)
fromOpcode SIToFP = (#const LLVMSIToFP)
fromOpcode FPTrunc = (#const LLVMFPTrunc)
fromOpcode FPExt = (#const LLVMFPExt)
fromOpcode PtrToInt = (#const LLVMPtrToInt)
fromOpcode IntToPtr = (#const LLVMIntToPtr)
fromOpcode BitCast = (#const LLVMBitCast)
fromOpcode ICmp = (#const LLVMICmp)
fromOpcode FCmp = (#const LLVMFCmp)
fromOpcode PHI = (#const LLVMPHI)
fromOpcode Call = (#const LLVMCall)
fromOpcode Select = (#const LLVMSelect)
fromOpcode VAArg = (#const LLVMVAArg)
fromOpcode ExtractElement = (#const LLVMExtractElement)
fromOpcode InsertElement = (#const LLVMInsertElement)
fromOpcode ShuffleVector = (#const LLVMShuffleVector)
fromOpcode ExtractValue = (#const LLVMExtractValue)
fromOpcode InsertValue = (#const LLVMInsertValue)
fromOpcode Fence = (#const LLVMFence)
fromOpcode AtomicCmpXchg = (#const LLVMAtomicCmpXchg)
fromOpcode AtomicRMW = (#const LLVMAtomicRMW)
fromOpcode Resume = (#const LLVMResume)
fromOpcode LandingPad = (#const LLVMLandingPad)

toOpcode :: COpcode -> Opcode
toOpcode c
  | c == (#const LLVMRet) = Ret
  | c == (#const LLVMBr) = Br
  | c == (#const LLVMSwitch) = Switch
  | c == (#const LLVMIndirectBr) = IndirectBr
  | c == (#const LLVMInvoke) = Invoke
  | c == (#const LLVMUnreachable) = Unreachable
  | c == (#const LLVMAdd) = Add
  | c == (#const LLVMFAdd) = FAdd
  | c == (#const LLVMSub) = Sub
  | c == (#const LLVMFSub) = FSub
  | c == (#const LLVMMul) = Mul
  | c == (#const LLVMFMul) = FMul
  | c == (#const LLVMUDiv) = UDiv
  | c == (#const LLVMSDiv) = SDiv
  | c == (#const LLVMFDiv) = FDiv
  | c == (#const LLVMURem) = URem
  | c == (#const LLVMSRem) = SRem
  | c == (#const LLVMFRem) = FRem
  | c == (#const LLVMShl) = Shl
  | c == (#const LLVMLShr) = LShr
  | c == (#const LLVMAShr) = AShr
  | c == (#const LLVMAnd) = And
  | c == (#const LLVMOr) = Or
  | c == (#const LLVMXor) = Xor
  | c == (#const LLVMAlloca) = Alloca
  | c == (#const LLVMLoad) = Load
  | c == (#const LLVMStore) = Store
  | c == (#const LLVMGetElementPtr) = GetElementPtr
  | c == (#const LLVMTrunc) = Trunc
  | c == (#const LLVMZExt) = ZExt
  | c == (#const LLVMSExt) = SExt
  | c == (#const LLVMFPToUI) = FPToUI
  | c == (#const LLVMFPToSI) = FPToSI
  | c == (#const LLVMUIToFP) = UIToFP
  | c == (#const LLVMSIToFP) = SIToFP
  | c == (#const LLVMFPTrunc) = FPTrunc
  | c == (#const LLVMFPExt) = FPExt
  | c == (#const LLVMPtrToInt) = PtrToInt
  | c == (#const LLVMIntToPtr) = IntToPtr
  | c == (#const LLVMBitCast) = BitCast
  | c == (#const LLVMICmp) = ICmp
  | c == (#const LLVMFCmp) = FCmp
  | c == (#const LLVMPHI) = PHI
  | c == (#const LLVMCall) = Call
  | c == (#const LLVMSelect) = Select
  | c == (#const LLVMVAArg) = VAArg
  | c == (#const LLVMExtractElement) = ExtractElement
  | c == (#const LLVMInsertElement) = InsertElement
  | c == (#const LLVMShuffleVector) = ShuffleVector
  | c == (#const LLVMExtractValue) = ExtractValue
  | c == (#const LLVMInsertValue) = InsertValue
  | c == (#const LLVMFence) = Fence
  | c == (#const LLVMAtomicCmpXchg) = AtomicCmpXchg
  | c == (#const LLVMAtomicRMW) = AtomicRMW
  | c == (#const LLVMResume) = Resume
  | c == (#const LLVMLandingPad) = LandingPad

type COpcode = CUInt

data Attribute =
    ZExtAttribute
  | SExtAttribute
  | NoReturnAttribute
  | InRegAttribute
  | StructRetAttribute
  | NoUnwindAttribute
  | NoAliasAttribute
  | ByValAttribute
  | NestAttribute
  | ReadNoneAttribute
  | ReadOnlyAttribute
  | NoInlineAttribute
  | AlwaysInlineAttribute
  | OptimizeForSizeAttribute
  | StackProtectAttribute
  | StackProtectReqAttribute
  | NoCaptureAttribute
  | NoRedZoneAttribute
  | NoImplicitFloatAttribute
  | NakedAttribute
    deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

fromAttribute :: Attribute -> CAttribute
fromAttribute ZExtAttribute = (#const LLVMZExtAttribute)
fromAttribute SExtAttribute = (#const LLVMSExtAttribute)
fromAttribute NoReturnAttribute = (#const LLVMNoReturnAttribute)
fromAttribute InRegAttribute = (#const LLVMInRegAttribute)
fromAttribute StructRetAttribute = (#const LLVMStructRetAttribute)
fromAttribute NoUnwindAttribute = (#const LLVMNoUnwindAttribute)
fromAttribute NoAliasAttribute = (#const LLVMNoAliasAttribute)
fromAttribute ByValAttribute = (#const LLVMByValAttribute)
fromAttribute NestAttribute = (#const LLVMNestAttribute)
fromAttribute ReadNoneAttribute = (#const LLVMReadNoneAttribute)
fromAttribute ReadOnlyAttribute = (#const LLVMReadOnlyAttribute)
fromAttribute NoInlineAttribute = (#const LLVMNoInlineAttribute)
fromAttribute AlwaysInlineAttribute = (#const LLVMAlwaysInlineAttribute)
fromAttribute OptimizeForSizeAttribute = (#const LLVMOptimizeForSizeAttribute)
fromAttribute StackProtectAttribute = (#const LLVMStackProtectAttribute)
fromAttribute StackProtectReqAttribute = (#const LLVMStackProtectReqAttribute)
fromAttribute NoCaptureAttribute = (#const LLVMNoCaptureAttribute)
fromAttribute NoRedZoneAttribute = (#const LLVMNoRedZoneAttribute)
fromAttribute NoImplicitFloatAttribute = (#const LLVMNoImplicitFloatAttribute)
fromAttribute NakedAttribute = (#const LLVMNakedAttribute)

toAttribute :: CAttribute -> Attribute
toAttribute c
  | c == (#const LLVMZExtAttribute) = ZExtAttribute
  | c == (#const LLVMSExtAttribute) = SExtAttribute
  | c == (#const LLVMNoReturnAttribute) = NoReturnAttribute
  | c == (#const LLVMInRegAttribute) = InRegAttribute
  | c == (#const LLVMStructRetAttribute) = StructRetAttribute
  | c == (#const LLVMNoUnwindAttribute) = NoUnwindAttribute
  | c == (#const LLVMNoAliasAttribute) = NoAliasAttribute
  | c == (#const LLVMByValAttribute) = ByValAttribute
  | c == (#const LLVMNestAttribute) = NestAttribute
  | c == (#const LLVMReadNoneAttribute) = ReadNoneAttribute
  | c == (#const LLVMReadOnlyAttribute) = ReadOnlyAttribute
  | c == (#const LLVMNoInlineAttribute) = NoInlineAttribute
  | c == (#const LLVMAlwaysInlineAttribute) = AlwaysInlineAttribute
  | c == (#const LLVMOptimizeForSizeAttribute) = OptimizeForSizeAttribute
  | c == (#const LLVMStackProtectAttribute) = StackProtectAttribute
  | c == (#const LLVMStackProtectReqAttribute) = StackProtectReqAttribute
  | c == (#const LLVMNoCaptureAttribute) = NoCaptureAttribute
  | c == (#const LLVMNoRedZoneAttribute) = NoRedZoneAttribute
  | c == (#const LLVMNoImplicitFloatAttribute) = NoImplicitFloatAttribute
  | c == (#const LLVMNakedAttribute) = NakedAttribute
  | otherwise = error "toAttribute: bad value"

type CAttribute = CUInt

data TypeKind =
    VoidTypeKind
  | FloatTypeKind
  | DoubleTypeKind
  | X86_FP80TypeKind
  | FP128TypeKind
  | PPC_FP128TypeKind
  | LabelTypeKind
  | IntegerTypeKind
  | FunctionTypeKind
  | StructTypeKind
  | ArrayTypeKind
  | PointerTypeKind
  | MetadataTypeKind
  | VectorTypeKind
  | X86_MMXTypeKind
    deriving (Eq, Ord, Show, Read, Typeable)

fromTypeKind :: TypeKind -> CUInt
fromTypeKind VoidTypeKind = (#const LLVMVoidTypeKind)
fromTypeKind FloatTypeKind = (#const LLVMFloatTypeKind)
fromTypeKind DoubleTypeKind = (#const LLVMDoubleTypeKind)
fromTypeKind X86_FP80TypeKind = (#const LLVMX86_FP80TypeKind)
fromTypeKind FP128TypeKind = (#const LLVMFP128TypeKind)
fromTypeKind PPC_FP128TypeKind = (#const LLVMPPC_FP128TypeKind)
fromTypeKind LabelTypeKind = (#const LLVMLabelTypeKind)
fromTypeKind IntegerTypeKind = (#const LLVMIntegerTypeKind)
fromTypeKind StructTypeKind = (#const LLVMStructTypeKind)
fromTypeKind ArrayTypeKind = (#const LLVMArrayTypeKind)
fromTypeKind PointerTypeKind = (#const LLVMPointerTypeKind)
fromTypeKind VectorTypeKind = (#const LLVMVectorTypeKind)
fromTypeKind MetadataTypeKind = (#const LLVMMetadataTypeKind)
fromTypeKind X86_MMXTypeKind = (#const LLVMX86_MMXTypeKind)

toTypeKind :: CUInt -> TypeKind
toTypeKind c
  | c == (#const LLVMVoidTypeKind) = VoidTypeKind
  | c == (#const LLVMFloatTypeKind) = FloatTypeKind
  | c == (#const LLVMDoubleTypeKind) = DoubleTypeKind
  | c == (#const LLVMX86_FP80TypeKind) = X86_FP80TypeKind
  | c == (#const LLVMFP128TypeKind) = FP128TypeKind
  | c == (#const LLVMPPC_FP128TypeKind) = PPC_FP128TypeKind
  | c == (#const LLVMLabelTypeKind) = LabelTypeKind
  | c == (#const LLVMIntegerTypeKind) = IntegerTypeKind
  | c == (#const LLVMStructTypeKind) = StructTypeKind
  | c == (#const LLVMArrayTypeKind) = ArrayTypeKind
  | c == (#const LLVMPointerTypeKind) = PointerTypeKind
  | c == (#const LLVMVectorTypeKind) = VectorTypeKind
  | c == (#const LLVMMetadataTypeKind) = MetadataTypeKind
  | c == (#const LLVMX86_MMXTypeKind) = X86_MMXTypeKind

data CallingConvention =
    C
  | Fast
  | Cold
  | X86StdCall
  | X86FastCall
  | GHC
  | Custom Word
    deriving (Show, Eq, Ord, Typeable)

fromCallingConvention :: CallingConvention -> CUInt
fromCallingConvention C = (#const LLVMCCallConv)
fromCallingConvention Fast = (#const LLVMFastCallConv)
fromCallingConvention Cold = (#const LLVMColdCallConv)
fromCallingConvention X86StdCall = (#const LLVMX86FastcallCallConv)
fromCallingConvention X86FastCall = (#const LLVMX86StdcallCallConv)
fromCallingConvention GHC = 10
fromCallingConvention (Custom c) = fromIntegral c

toCallingConvention :: CUInt -> CallingConvention
toCallingConvention c
  | c == (#const LLVMCCallConv) = C
  | c == (#const LLVMFastCallConv) = Fast
  | c == (#const LLVMColdCallConv) = Cold
  | c == (#const LLVMX86StdcallCallConv) = X86StdCall
  | c == (#const LLVMX86FastcallCallConv) = X86FastCall
  | c == 10 = GHC
  | otherwise = Custom (fromIntegral c)

-- | An enumeration for the kinds of linkage for global values.
data Linkage =
    ExternalLinkage     -- ^Externally visible function
  | AvailableExternallyLinkage 
  | LinkOnceAnyLinkage  -- ^Keep one copy of function when linking (inline)
  | LinkOnceODRLinkage  -- ^Same, but only replaced by something equivalent.
  | WeakAnyLinkage      -- ^Keep one copy of named function when linking (weak)
  | WeakODRLinkage      -- ^Same, but only replaced by something equivalent.
  | AppendingLinkage    -- ^Special purpose, only applies to global arrays
  | InternalLinkage     -- ^Rename collisions when linking (static functions)
  | PrivateLinkage      -- ^Like Internal, but omit from symbol table
  | DLLImportLinkage    -- ^Function to be imported from DLL
  | DLLExportLinkage    -- ^Function to be accessible from DLL
  | ExternalWeakLinkage -- ^ExternalWeak linkage description
  | GhostLinkage        -- ^Stand-in functions for streaming fns from BC files
  | CommonLinkage       -- ^Tentative definitions
  | LinkerPrivateLinkage -- ^Like Private, but linker removes.
    deriving (Show, Eq, Ord, Enum, Typeable)

fromLinkage :: Linkage -> CLinkage
fromLinkage ExternalLinkage = (#const LLVMExternalLinkage)
fromLinkage AvailableExternallyLinkage =
  (#const LLVMAvailableExternallyLinkage )
fromLinkage LinkOnceAnyLinkage = (#const LLVMLinkOnceAnyLinkage)
fromLinkage LinkOnceODRLinkage = (#const LLVMLinkOnceODRLinkage)
fromLinkage WeakAnyLinkage = (#const LLVMWeakAnyLinkage)
fromLinkage WeakODRLinkage = (#const LLVMWeakODRLinkage)
fromLinkage AppendingLinkage = (#const LLVMAppendingLinkage)
fromLinkage InternalLinkage = (#const LLVMInternalLinkage)
fromLinkage PrivateLinkage = (#const LLVMPrivateLinkage)
fromLinkage DLLImportLinkage = (#const LLVMDLLImportLinkage)
fromLinkage DLLExportLinkage = (#const LLVMDLLExportLinkage)
fromLinkage ExternalWeakLinkage = (#const LLVMExternalWeakLinkage)
fromLinkage GhostLinkage = (#const LLVMGhostLinkage)
fromLinkage CommonLinkage = (#const LLVMCommonLinkage)
fromLinkage LinkerPrivateLinkage = (#const LLVMLinkerPrivateLinkage)

toLinkage :: CLinkage -> Linkage
toLinkage c
  | c == (#const LLVMExternalLinkage) = ExternalLinkage
  | c == (#const LLVMAvailableExternallyLinkage) = AvailableExternallyLinkage 
  | c == (#const LLVMLinkOnceAnyLinkage) = LinkOnceAnyLinkage
  | c == (#const LLVMLinkOnceODRLinkage) = LinkOnceODRLinkage
  | c == (#const LLVMWeakAnyLinkage) = WeakAnyLinkage
  | c == (#const LLVMWeakODRLinkage) = WeakODRLinkage
  | c == (#const LLVMAppendingLinkage) = AppendingLinkage
  | c == (#const LLVMInternalLinkage) = InternalLinkage
  | c == (#const LLVMPrivateLinkage) = PrivateLinkage
  | c == (#const LLVMDLLImportLinkage) = DLLImportLinkage
  | c == (#const LLVMDLLExportLinkage) = DLLExportLinkage
  | c == (#const LLVMExternalWeakLinkage) = ExternalWeakLinkage
  | c == (#const LLVMGhostLinkage) = GhostLinkage
  | c == (#const LLVMCommonLinkage) = CommonLinkage
  | c == (#const LLVMLinkerPrivateLinkage) = LinkerPrivateLinkage
  | otherwise = error "toLinkage: bad value"

type CLinkage = CUInt

-- |An enumeration for the kinds of visibility of global values.
data Visibility =
    DefaultVisibility   -- ^The GV is visible
  | HiddenVisibility    -- ^The GV is hidden
  | ProtectedVisibility -- ^The GV is protected
    deriving (Show, Eq, Ord, Enum)

fromVisibility :: Visibility -> CUInt
fromVisibility DefaultVisibility = (#const LLVMDefaultVisibility)
fromVisibility HiddenVisibility = (#const LLVMHiddenVisibility)
fromVisibility ProtectedVisibility = (#const LLVMProtectedVisibility)

toVisibility :: CUInt -> Visibility
toVisibility c
  | c == (#const LLVMDefaultVisibility) = DefaultVisibility
  | c == (#const LLVMHiddenVisibility) = HiddenVisibility
  | c == (#const LLVMProtectedVisibility) = ProtectedVisibility
  | otherwise = error "toVisibility: bad value"

data IntPredicate =
    IntEQ
  | IntNE
  | IntUGT
  | IntUGE
  | IntULT
  | IntULE
  | IntSGT
  | IntSGE
  | IntSLT
  | IntSLE

fromIntPredicate :: IntPredicate -> CIntPredicate
fromIntPredicate IntEQ = (#const LLVMIntEQ)
fromIntPredicate IntNE = (#const LLVMIntNE)
fromIntPredicate IntUGT = (#const LLVMIntUGT)
fromIntPredicate IntUGE = (#const LLVMIntUGE)
fromIntPredicate IntULT = (#const LLVMIntULT)
fromIntPredicate IntULE = (#const LLVMIntULE)
fromIntPredicate IntSGT = (#const LLVMIntSGT)
fromIntPredicate IntSGE = (#const LLVMIntSGE)
fromIntPredicate IntSLT = (#const LLVMIntSLT)
fromIntPredicate IntSLE = (#const LLVMIntSLE)

toIntPredicate :: CIntPredicate -> IntPredicate
toIntPredicate c
  | c == (#const LLVMIntEQ) = IntEQ
  | c == (#const LLVMIntNE) = IntNE
  | c == (#const LLVMIntUGT) = IntUGT
  | c == (#const LLVMIntUGE) = IntUGE
  | c == (#const LLVMIntULT) = IntULT
  | c == (#const LLVMIntULE) = IntULE
  | c == (#const LLVMIntSGT) = IntSGT
  | c == (#const LLVMIntSGE) = IntSGE
  | c == (#const LLVMIntSLT) = IntSLT
  | c == (#const LLVMIntSLE) = IntSLE
  | otherwise = error ("Invalid IntPredicate value " ++ show c)

type CIntPredicate = CUInt

data RealPredicate =
    RealFalse
  | RealTrue
  | RealOEQ
  | RealOGT
  | RealOGE
  | RealOLT
  | RealOLE
  | RealONE
  | RealUNO
  | RealUEQ
  | RealUGT
  | RealUGE
  | RealULT
  | RealULE
  | RealUNE

fromRealPredicate :: RealPredicate -> CRealPredicate
fromRealPredicate RealFalse = (#const LLVMRealPredicateFalse)
fromRealPredicate RealTrue = (#const LLVMRealPredicateTrue)
fromRealPredicate RealOEQ = (#const LLVMRealOEQ)
fromRealPredicate RealOGT = (#const LLVMRealOGT)
fromRealPredicate RealOGE = (#const LLVMRealOGE)
fromRealPredicate RealOLT = (#const LLVMRealOLT)
fromRealPredicate RealOLE = (#const LLVMRealOLE)
fromRealPredicate RealONE = (#const LLVMRealONE)
fromRealPredicate RealUEQ = (#const LLVMRealUEQ)
fromRealPredicate RealUGT = (#const LLVMRealUGT)
fromRealPredicate RealUGE = (#const LLVMRealUGE)
fromRealPredicate RealULT = (#const LLVMRealULT)
fromRealPredicate RealULE = (#const LLVMRealULE)
fromRealPredicate RealUNE = (#const LLVMRealUNE)

toRealPredicate :: CRealPredicate -> RealPredicate
toRealPredicate c
  | c == (#const LLVMRealPredicateFalse) = RealFalse
  | c == (#const LLVMRealPredicateTrue) = RealTrue
  | c == (#const LLVMRealOEQ) = RealOEQ
  | c == (#const LLVMRealOGT) = RealOGT
  | c == (#const LLVMRealOGE) = RealOGE
  | c == (#const LLVMRealOLT) = RealOLT
  | c == (#const LLVMRealOLE) = RealOLE
  | c == (#const LLVMRealONE) = RealONE
  | c == (#const LLVMRealUEQ) = RealUEQ
  | c == (#const LLVMRealUGT) = RealUGT
  | c == (#const LLVMRealUGE) = RealUGE
  | c == (#const LLVMRealULT) = RealULT
  | c == (#const LLVMRealULE) = RealULE
  | c == (#const LLVMRealUNE) = RealUNE

type CRealPredicate = CUInt

-- * Error Handling
foreign import ccall unsafe "LLVMDisposeMessage"
  disposeMessage :: CString -> IO ()

-- * Contexts

-- | Create a new context.
--
-- Every call to this function should be paired with a call to
-- contextDispose or the context will leak memory.
foreign import ccall unsafe "LLVMContextCreate"
  contextCreate :: IO ContextRef

-- | Obtain the global context instance.
foreign import ccall unsafe "LLVMGetGlobalContext"
  getGlobalContext :: IO ContextRef

-- | Destroy a context instance.
--
-- This should be called for every call to contextCreate() or memory
-- will be leaked.
foreign import ccall unsafe "LLVMContextDispose"
  contextDispose :: ContextRef
                    -- ^ Context to destroy.
                    -> IO ()

foreign import ccall unsafe "LLVMGetMDKindIDInContext"
  getMDKindIDInContext :: ContextRef
                          -- ^ Context
                          -> CString
                          -- ^ Name
                          -> CUInt
                          -- ^ String length
                          -> IO CUInt

foreign import ccall unsafe "LLVMGetMDKindID"
  getMDKindID :: CString
              -- ^ Metadata name
              -> CUInt
              -- ^ String length
              -> IO CUInt


-- * Modules

-- | Create a new, empty module in the global context.
--
-- This is equivalent to calling moduleCreateWithNameInContext with
-- gGetGlobalContext() as the context parameter.
--
-- Every invocation should be paired with LLVMDisposeModule() or memory
-- will be leaked.
foreign import ccall unsafe "LLVMModuleCreateWithName"
  moduleCreateWithName :: CString
                       -- ^ Module name
                       -> IO ModuleRef

-- | Create a new, empty module in a specific context.
--
-- Every invocation should be paired with LLVMDisposeModule() or memory
-- will be leaked.
foreign import ccall unsafe "LLVMModuleCreateWithNameInContext"
  moduleCreateWithNameInContext :: CString
                                -- ^ Module name
                                -> ContextRef
                                -- ^ Context
                                -> IO ModuleRef

-- | Destroy a module instance.
--
-- This must be called for every created module or memory will be
-- leaked.
foreign import ccall unsafe "LLVMDisposeModule"
  disposeModule :: ModuleRef
                -- ^ Module to destroy
                -> IO ()

foreign import ccall unsafe "&LLVMDisposeModule"
  ptrDisposeModule :: FunPtr (ModuleRef -> IO ())

-- ** Data Layout

-- | Obtain the data layout for a module.  See Module::getDataLayout()
foreign import ccall unsafe "LLVMGetDataLayout"
  getDataLayout :: ModuleRef
                -- ^ Module
                -> IO CString

-- | Set the data layout for a module.  See Module::getDataLayout()
foreign import ccall unsafe "LLVMSetDataLayout"
  setDataLayout :: ModuleRef
                -- ^ Module
                -> CString
                -- ^ Data layout string.
                -> IO ()

-- ** Targets

-- | Obtain the target triple for a module.
foreign import ccall unsafe "LLVMGetTarget"
  getTarget :: ModuleRef
            -- ^ Module
            -> IO CString

-- | Set the target triple for a module.
foreign import ccall unsafe "LLVMSetTarget"
  setTarget :: ModuleRef
            -- ^ Module
            -> CString
            -- ^ Target
            -> IO ()

-- ** Output

-- | Dump a representation of a module to stderr.
foreign import ccall unsafe "LLVMDumpModule"
  dumpModule :: ModuleRef -> IO ()

-- ** Types

-- | Obtain a Type from a module by its registered name.
foreign import ccall unsafe "LLVMGetTypeByName"
  getTypeByName :: ModuleRef
                -- ^ Module
                -> CString
                -- ^ Type name
                -> IO TypeRef

-- ** Metadata

-- | Obtain the number of operands for named metadata in a module.
foreign import ccall unsafe "LLVMGetNamedMetadataNumOperands"
  getNamedMetadataNumOperands :: ModuleRef
                              -- ^ Module
                              -> CString
                              -- ^ Metadata name
                              -> IO CUInt
                              -- ^ Number of operands

-- | Obtain the named metadata operands for a module.
foreign import ccall unsafe "LLVMGetNamedMetadataOperands"
  getNamedMetadataOperands :: ModuleRef
                           -- ^ Module
                           -> CString
                           -- ^ Metadata name
                           -> Ptr ValueRef
                           -- ^ Array at least (number of operands) long
                           -> IO ()

-- | Add an operand to named metadata.
foreign import ccall unsafe "LLVMAddNamedMetadataOperand"
  addNamedMetadataOperand :: ModuleRef
                          -- ^ Module
                          -> CString
                          -- ^ Metadata name
                          -> ValueRef
                          -- ^ Operand (must be a metadata node)
                          -> IO ()

-- ** Functions

-- | Add a function to a module under a specified name.
foreign import ccall unsafe "LLVMAddFunction"
  addFunction :: ModuleRef
              -- ^ Module
              -> CString
              -- ^ Function name
              -> TypeRef
              -- ^ Function type
              -> IO ValueRef
              -- ^ Function value


-- | Obtain a Function value from a Module by its name.
--
-- The returned value corresponds to a llvm::Function value.
foreign import ccall unsafe "LLVMGetNamedFunction"
  getNamedFunction :: ModuleRef
                   -- ^ Module
                   -> CString
                   -- ^ Function name
                   -> IO ValueRef
                   -- ^ Function value (@nullPtr@ if not found)

-- | Obtain an iterator to the first Function in a Module.
foreign import ccall unsafe "LLVMGetFirstFunction"
  getFirstFunction :: ModuleRef -> IO ValueRef

-- | Obtain an iterator to the last Function in a Module.
foreign import ccall unsafe "LLVMGetLastFunction"
  getLastFunction :: ModuleRef -> IO ValueRef

-- | Advance a Function iterator to the next Function.
--
-- Returns @nullPtr@ if the iterator was already at the end and there
-- are no more functions.
foreign import ccall unsafe "LLVMGetNextFunction"
  getNextFunction :: ValueRef -> IO ValueRef

-- | Decrement a Function iterator to the previous Function.
--
-- Returns @nullPtr@ if the iterator was already at the beginning and
-- there are no previous functions.
foreign import ccall unsafe "LLVMGetPreviousFunction"
  getPreviousFunction :: ValueRef -> IO ValueRef

-- ** Other module functions

-- | Set inline assembly for a module.
foreign import ccall unsafe "LLVMSetModuleInlineAsm"
  setModuleInlineAsm :: ModuleRef
                     -- ^ Module
                     -> CString
                     -- ^ Assembly string
                     -> IO ()

-- | Obtain the context to which this module is associated.
foreign import ccall unsafe "LLVMGetModuleContext"
  getModuleContext :: ModuleRef -> IO ContextRef

-- * Type

-- | Obtain the enumerated type of a Type instance.
foreign import ccall unsafe "LLVMGetTypeKind"
  getTypeKind :: TypeRef -> IO CUInt

-- | Whether the type has a known size.
--
-- Things that don't have a size are abstract types, labels, and void.
foreign import ccall unsafe "LLVMTypeIsSized"
  typeIsSized :: TypeRef -> IO CInt

-- | Obtain the context to which this type instance is associated.
foreign import ccall unsafe "LLVMGetTypeContext"
  getTypeContext :: TypeRef -> IO ContextRef

-- ** Integer types
foreign import ccall unsafe "LLVMInt1TypeInContext"
  int1TypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMInt8TypeInContext"
  int8TypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMInt16TypeInContext"
  int16TypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMInt32TypeInContext"
  int32TypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMInt64TypeInContext"
  int64TypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMIntTypeInContext"
  intTypeInContext :: ContextRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMInt1Type" int1Type :: TypeRef
foreign import ccall unsafe "LLVMInt8Type" int8Type :: TypeRef
foreign import ccall unsafe "LLVMInt16Type" int16Type :: TypeRef
foreign import ccall unsafe "LLVMInt32Type" int32Type :: TypeRef
foreign import ccall unsafe "LLVMInt64Type" int64Type :: TypeRef
foreign import ccall unsafe "LLVMIntType" intType :: CUInt -> TypeRef
foreign import ccall unsafe "LLVMGetIntTypeWidth"
  getIntTypeWidth :: TypeRef -> IO CUInt

-- ** Floating Point types

foreign import ccall unsafe "LLVMHalfTypeInContext"
  halfTypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMFloatTypeInContext"
  floatTypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMDoubleTypeInContext"
  doubleTypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMX86FP80TypeInContext"
  x86FP80TypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMFP128TypeInContext"
  fp128TypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMPPCFP128TypeInContext"
  ppcFP128TypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMHalfType" halfType :: TypeRef
foreign import ccall unsafe "LLVMFloatType" floatType :: TypeRef
foreign import ccall unsafe "LLVMDoubleType" doubleType :: TypeRef
foreign import ccall unsafe "LLVMX86FP80Type" x86FP80Type :: TypeRef
foreign import ccall unsafe "LLVMFP128Type" fp128Type :: TypeRef
foreign import ccall unsafe "LLVMPPCFP128Type" ppcFP128Type :: TypeRef

-- ** Function types

-- | Obtain a function type consisting of a specified signature.
--
-- The function is defined as a tuple of a return Type, a list of
-- parameter types, and whether the function is variadic.
foreign import ccall unsafe "LLVMFunctionType"
  functionType :: TypeRef
               -- ^ Return type
               -> Ptr TypeRef
               -- ^ Array of argument types
               -> CUInt
               -- ^ Number of elements in array
               -> CInt
               -- ^ Non-zero if function is varargs
               -> TypeRef
               -- ^ The function type

-- | Returns whether a function type is variadic.
foreign import ccall unsafe "LLVMIsFunctionVarArg"
  isFunctionVarArg :: TypeRef -> IO CInt

-- | Obtain the Type this function Type returns.
foreign import ccall unsafe "LLVMGetReturnType"
  getReturnType :: TypeRef -> IO TypeRef

-- | Obtain the number of parameters this function accepts.
foreign import ccall unsafe "LLVMCountParamTypes"
  countParamTypes :: TypeRef -> IO CUInt

-- | Obtain the types of a function's parameters.
foreign import ccall unsafe "LLVMGetParamTypes"
  getParamTypes :: TypeRef
                -- ^ Function type
                -> Ptr TypeRef
                -- ^ Array at least as long as the number of
                -- parameters
                -> IO ()

-- ** Struct Type

-- | Create a new structure type in a context.
--
-- A structure is specified by a list of inner elements/types and
-- whether these can be packed together.
foreign import ccall unsafe "LLVMStructTypeInContext"
  structTypeInContext :: ContextRef
                      -- ^ Context value
                      -> Ptr TypeRef
                      -- ^ Array of element types
                      -> CUInt
                      -- ^ Number of elements in the array
                      -> CInt
                      -- ^ Boolean value, indicating whether the
                      -- structure is packed
                      -> IO TypeRef
                      -- ^ Structure type

-- | Create a new structure type in the global context.
foreign import ccall unsafe "LLVMStructType"
  structType :: Ptr TypeRef
             -- ^ Array of element types
             -> CUInt
             -- ^ Number of elements in the array
             -> CInt
             -- ^ Boolean value, indicating whether the structure is
             -- packed
             -> TypeRef
             -- ^ Structure type

-- | Create an empty structure in a context having a specified name.
foreign import ccall unsafe "LLVMStructCreateNamed"
  structCreateNamed :: ContextRef
                    -- ^ Context
                    -> CString
                    -- ^ Structure name
                    -> IO TypeRef
                    -- ^ Structure type

-- | Obtain the name of a structure.
foreign import ccall unsafe "LLVMGetStructName"
  getStructName :: TypeRef -> CString

-- | Set the contents of a structure type.
foreign import ccall unsafe "LLVMStructSetBody"
  structSetBody :: TypeRef
                -- ^ Structure type
                -> Ptr TypeRef
                -- ^ Array of element types
                -> CUInt
                -- ^ Number of array elements
                -> CInt
                -- ^ Boolean value indicating whether the structure is
                -- packed
                -> IO ()

-- | Get the number of elements defined inside the structure.
foreign import ccall unsafe "LLVMCountStructElementTypes"
    countStructElementTypes :: TypeRef -> CUInt

-- | Get the elements within a structure.
foreign import ccall unsafe "LLVMGetStructElementTypes"
  getStructElementTypes :: TypeRef
                        -- ^ Structure type
                        -> Ptr TypeRef
                        -- ^ Array at least as long as the number of
                        -- elements
                        -> IO ()

-- | Determine whether a structure is packed.
foreign import ccall unsafe "LLVMIsPackedStruct"
  isPackedStruct :: TypeRef -> CInt

-- | Determine whether a structure is opaque.
foreign import ccall unsafe "LLVMIsOpaqueStruct"
  isOpaqueStruct :: TypeRef -> CInt

-- ** Array, pointer, and vector types

-- | Obtain the type of elements within a sequential type.
--
-- This works on array, vector, and pointer types.
foreign import ccall unsafe "LLVMGetElementType"
  getElementType :: TypeRef -> IO TypeRef

-- | Create a fixed size array type that refers to a specific type.
--
-- The created type will exist in the context that its element type
-- exists in.
foreign import ccall unsafe "LLVMArrayType"
  arrayType :: TypeRef
            -- ^ Element type
            -> CUInt
            -- ^ Number of elements
            -> TypeRef
            -- ^ Array type

-- | Obtain the length of an array type.
--
-- This only works on types that represent arrays.
foreign import ccall unsafe "LLVMGetArrayLength"
  getArrayLength :: TypeRef -> IO CUInt

-- | Create a pointer type that points to a defined type.
--
-- The created type will exist in the context that its pointee type
-- exists in.
foreign import ccall unsafe "LLVMPointerType"
  pointerType :: TypeRef
              -- ^ Pointed-to type
              -> CUInt
              -- ^ Address space
              -> TypeRef
              -- ^ Pointer type

-- | Obtain the address space of a pointer type.
--
-- This only works on types that represent pointers.
foreign import ccall unsafe "LLVMGetPointerAddressSpace"
  getPointerAddressSpace :: TypeRef -> IO CUInt

-- | Create a vector type that contains a defined type and has a specific
-- number of elements.
--
-- The created type will exist in the context thats its element type
-- exists in.
foreign import ccall unsafe "LLVMVectorType"
  vectorType :: TypeRef
             -- ^ Element type
             -> CUInt
             -- ^ Element count
             -> TypeRef
             -- ^ Vector type

-- | Obtain the number of elements in a vector type.
--
-- This only works on types that represent vectors.
foreign import ccall unsafe "LLVMGetVectorSize"
  getVectorSize :: TypeRef -> IO CUInt

-- ** Other types

-- | Create a void type in a context.
foreign import ccall unsafe "LLVMVoidTypeInContext"
  voidTypeInContext :: ContextRef -> IO TypeRef

-- | Create a label type in a context.
foreign import ccall unsafe "LLVMLabelTypeInContext"
  labelTypeInContext :: ContextRef -> IO TypeRef

-- | Create a X86 MMX type in a context.
foreign import ccall unsafe "LLVMX86MMXTypeInContext"
  x86MMXTypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMVoidType" voidType :: TypeRef
foreign import ccall unsafe "LLVMLabelType" labelType :: TypeRef
foreign import ccall unsafe "LLVMX86MMXType" x86MMXType :: TypeRef

-- * Values
foreign import ccall unsafe "LLVMIsAArgument"
  isAArgument :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsABasicBlock"
  isABasicBlock :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAInlineAsm"
  isAInlineAsm :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAMDNode"
  isAMDNode :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAMDString"
  isAMDString :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAUser"
  isAUser :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAConstant"
  isAConstant :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsABlockAddress"
  isABlockAddress :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAConstantAggregateZero"
  isAConstantAggregateZero :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAConstantArray"
  isAConstantArray :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAConstantExpr"
  isAConstantExpr :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAConstantFP"
  isAConstantFP :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAConstantInt"
  isAConstantInt :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAConstantPointerNull"
  isAConstantPointerNull :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAConstantStruct"
  isAConstantStruct :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAConstantVector"
  isAConstantVector :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAGlobalValue"
  isAGlobalValue :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAFunction"
  isAFunction :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAGlobalAlias"
  isAGlobalAlias :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAGlobalVariable"
  isAGlobalVariable :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAUndefValue"
  isAUndefValue :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAInstruction"
  isAInstruction :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsABinaryOperator"
  isABinaryOperator :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsACallInst"
  isACallInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAIntrinsicInst"
  isAIntrinsicInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsADbgInfoIntrinsic"
  isADbgInfoIntrinsic :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsADbgDeclareInst"
  isADbgDeclareInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAMemIntrinsic"
  isAMemIntrinsic :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAMemCpyInst"
  isAMemCpyInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAMemMoveInst"
  isAMemMoveInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAMemSetInst"
  isAMemSetInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsACmpInst"
  isACmpInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAFCmpInst"
  isAFCmpInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAICmpInst"
  isAICmpInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAExtractElementInst"
  isAExtractElementInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAGetElementPtrInst"
  isAGetElementPtrInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAInsertElementInst"
  isAInsertElementInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAInsertValueInst"
  isAInsertValueInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsALandingPadInst"
  isALandingPadInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAPHINode"
  isAPHINode :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsASelectInst"
  isASelectInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAShuffleVectorInst"
  isAShuffleVectorInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAStoreInst"
  isAStoreInst :: ValueRef -> IO Bool 

foreign import ccall unsafe "LLVMIsATerminatorInst"
  isATerminatorInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsABranchInst"
  isABranchInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAIndirectBrInst"
  isAIndirectBrInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAInvokeInst"
  isAInvokeInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAReturnInst"
  isAReturnInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsASwitchInst"
  isASwitchInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAUnreachableInst"
  isAUnreachableInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAResumeInst"
  isAResumeInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAUnaryInstruction"
  isAUnaryInstruction :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAAllocaInst"
  isAAllocaInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsACastInst"
  isACastInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsABitCastInst"
  isABitCastInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAFPExtInst"
  isAFPExtInst :: ValueRef -> IO Bool 

foreign import ccall unsafe "LLVMIsAFPToSIInst"
  isAFPToSIInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAFPToUIInst"
  isAFPToUIInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAFPTruncInst"
  isAFPTruncInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAIntToPtrInst"
  isAIntToPtrInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAPtrToIntInst"
  isAPtrToIntInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsASExtInst"
  isASExtInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsASIToFPInst"
  isASIToFPInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsATruncInst"
  isATruncInst :: ValueRef -> IO Bool 

foreign import ccall unsafe "LLVMIsAUIToFPInst"
  isAUIToFPInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAZExtInst"
  isAZExtInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAExtractValueInst"
  isAExtractValueInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsALoadInst"
  isALoadInst :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsAVAArgInst"
  isAVAArgInst :: ValueRef -> IO Bool

-- | Obtain the type of a value.
foreign import ccall unsafe "LLVMTypeOf"
  typeOf :: ValueRef -> IO TypeRef

-- | Obtain the string name of a value.
foreign import ccall unsafe "LLVMGetValueName"
  getValueName :: ValueRef -> IO CString

-- | Set the string name of a value.
foreign import ccall unsafe "LLVMSetValueName"
  setValueName :: ValueRef -> CString -> IO ()

-- | Dump a representation of a value to stderr.
foreign import ccall unsafe "LLVMDumpValue"
  dumpValue :: ValueRef -> IO ()

-- | Replace all uses of a value with another one.
foreign import ccall unsafe "LLVMReplaceAllUsesWith"
  replaceAllUsesWith :: ValueRef
                     -- ^ Old value
                     -> ValueRef
                     -- ^ New value
                     -> IO ()

-- | Determine whether the specified constant instance is constant.
foreign import ccall unsafe "LLVMIsConstant"
  isConstant :: ValueRef -> IO CInt

-- | Determine whether a value instance is undefined.
foreign import ccall unsafe "LLVMIsUndef"
  isUndef :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMIsNull"
  isNull :: ValueRef -> IO CInt

-- ** Uses
-- | Obtain the first use of a value.
foreign import ccall unsafe "LLVMGetFirstUse"
  getFirstUse :: ValueRef -> IO UseRef

-- | Obtain the next use of a value.
--
-- This effectively advances the iterator. It returns @nullPtr@ if you are on
-- the final use and no more are available.
foreign import ccall unsafe "LLVMGetNextUse"
  getNextUse :: UseRef -> IO UseRef

-- | Obtain the user value for a user.
foreign import ccall unsafe "LLVMGetUser"
  getUser :: UseRef -> IO ValueRef

-- | Obtain the value this use corresponds to.
foreign import ccall unsafe "LLVMGetUsedValue"
  getUsedValue :: UseRef -> IO ValueRef

-- ** Users

-- | Obtain an operand at a specific index in a llvm::User value.
foreign import ccall unsafe "LLVMGetOperand"
  getOperand :: ValueRef -> CUInt -> IO ValueRef

-- | Set an operand at a specific index in a user value.
foreign import ccall unsafe "LLVMSetOperand"
  setOperand :: ValueRef -> CUInt -> ValueRef -> IO ()

-- | Obtain the number of operands in a llvm::User value.
foreign import ccall unsafe "LLVMGetNumOperands"
  getNumOperands :: ValueRef -> IO CUInt

-- * Constants

-- | Obtain a constant value referring to the null instance of a type.
foreign import ccall unsafe "LLVMConstNull"
  constNull :: TypeRef -> ValueRef

-- | Obtain a constant value referring to the instance of a type
-- consisting of all ones.
--
-- This is only valid for integer types.
foreign import ccall unsafe "LLVMConstAllOnes"
  constAllOnes :: TypeRef -> ValueRef

-- | Obtain a constant value referring to an undefined value of a
-- type.
foreign import ccall unsafe "LLVMGetUndef"
  getUndef :: TypeRef -> ValueRef

-- | Obtain a constant that is a constant pointer pointing to NULL for
-- a specified type.
foreign import ccall unsafe "LLVMConstPointerNull"
  constPointerNull :: TypeRef -> IO ValueRef

-- ** Scalar Constants

-- | Obtain a constant value for an integer type.
--
-- The returned value corresponds to a llvm::ConstantInt.
foreign import ccall unsafe "LLVMConstInt"
  constInt :: TypeRef
           -- ^ Type, must be an integer type
           -> CULLong
           -- ^ Integer value
           -> CInt
           -- ^ Boolean value indicating whether to sign extend
           -> ValueRef
           -- ^ Constant value

-- | Obtain a constant value for an integer of arbitrary precision.
foreign import ccall unsafe "LLVMConstIntOfArbitraryPrecision"
  constIntOfArbitraryPrecision :: TypeRef
                               -- ^ Type, must be an integer type
                               -> CUInt
                               -- ^ Length of array
                               -> Ptr CULLong
                               -- ^ Array of integer data
                               -> ValueRef
                               -- ^ Integer value

-- | Obtain a constant value for an integer parsed from a string.
foreign import ccall unsafe "LLVMConstIntOfString"
  constIntOfString :: TypeRef
                   -- ^ Type, must be an integer type
                   -> CString
                   -- ^ String literal
                   -> CUChar
                   -- ^ Radix
                   -> IO ValueRef
                   -- ^ Integer constant

-- | Obtain a constant value for an integer parsed from a string with
-- specified length.  It is preferable to call this version if the
-- string's length is known.
foreign import ccall unsafe "LLVMConstIntOfStringAndSize"
  constIntOfStringAndSize :: TypeRef
                          -- ^ Type, must be an integer type
                          -> CString
                          -- ^ String literal
                          -> CUInt
                          -- ^ String length
                          -> CUChar
                          -- ^ Radix
                          -> IO ValueRef
                          -- ^ Integer constant

-- | Obtain a constant value referring to a double floating point value.
foreign import ccall unsafe "LLVMConstReal"
  constReal :: TypeRef
            -- ^ Type, must be a floating point type
            -> CDouble
            -- ^ Floating point value
            -> ValueRef
            -- ^ Floating point constant

-- | Obtain a constant for a floating point value parsed from a string.
foreign import ccall unsafe "LLVMConstRealOfString"
  constRealOfString :: TypeRef
                    -- ^ Type, must be a floating point type
                    -> CString
                    -- ^ String literal
                    -> IO ValueRef
                    -- ^ Floating point constant

-- | Obtain a constant for a floating point value parsed from a
-- string.  It is preferable to call this version if the string's
-- length is known.
foreign import ccall unsafe "LLVMConstRealOfStringAndSize"
  constRealOfStringAndSize :: TypeRef
                           -- ^ Type, must be a floating point type
                           -> CString
                           -- ^ String literal
                           -> CUInt
                           -- ^ String length
                           -> IO ValueRef
                           -- ^ Floating point constant

-- | Obtain the zero extended value for an integer constant value.
foreign import ccall unsafe "LLVMConstIntGetZExtValue"
  constIntGetZExtValue :: ValueRef -> IO CULLong

-- | Obtain the sign extended value for an integer constant value.
foreign import ccall unsafe "LLVMConstIntGetSExtValue"
  constIntGetSExtValue :: ValueRef -> IO CLLong

-- ** Composite Constants

-- | Create a ConstantDataSequential and initialize it with a string.
foreign import ccall unsafe "LLVMConstStringInContext"
  constStringInContext :: ContextRef
                       -- ^ Context
                       -> CString
                       -- ^ String constant
                       -> CUInt
                       -- ^ String length
                       -> CInt
                       -- ^ Boolean value indicating whether the
                       -- string is null-terminated.
                       -> IO ValueRef
                       -- ^ String constant

-- | Create a ConstantDataSequential and initialize it with a string
-- in the global context
foreign import ccall unsafe "LLVMConstString"
  constString :: CString
              -- ^ String constant
              -> CUInt
              -- ^ String length
              -> CInt
              -- ^ Boolean value indicating whether the string is
              -- null-terminated.
              -> IO ValueRef
              -- ^ String constant

-- | Create an anonymous ConstantStruct with the specified values.
foreign import ccall unsafe "LLVMConstStructInContext"
  constStructInContext :: ContextRef
                       -- ^ Context
                       -> Ptr ValueRef
                       -- ^ Array of elements
                       -> CUInt
                       -- ^ Number of elements
                       -> CInt
                       -- ^ Boolean value indicating whether the
                       -- structure is packed
                       -> IO ValueRef
                       -- ^ Constant value

-- | Create an anonymous ConstantStruct with the specified values in
-- the global context
foreign import ccall unsafe "LLVMConstStruct"
  constStruct :: Ptr ValueRef
              -- ^ Array of elements
              -> CUInt
              -- ^ Number of elements
              -> CInt
              -- ^ Boolean value indicating whether the structure is
              -- packed
              -> ValueRef
              -- ^ Constant value

-- | Create a ConstantArray from values.
foreign import ccall unsafe "LLVMConstArray"
  constArray :: TypeRef
             -- ^ Element type
             -> Ptr ValueRef
             -- ^ Array of element values
             -> CUInt
             -- ^ Number of elements
             -> ValueRef
             -- ^ Constant value

-- | Create a non-anonymous ConstantStruct from values.
foreign import ccall unsafe "LLVMConstNamedStruct"
  constNamedStruct :: TypeRef
                   -- ^ Structure type
                   -> Ptr ValueRef
                   -- ^ Array of elements
                   -> CUInt
                   -- ^ Number of elements
                   -> IO ValueRef
                   -- ^ Constant value

-- | Create a ConstantVector from values.
foreign import ccall unsafe "LLVMConstVector"
  constVector :: Ptr ValueRef
              -- ^ Array of elements
              -> CUInt
              -- ^ Number of elements
              -> ValueRef
              -- ^ Constant value

-- ** Constant expressions
foreign import ccall unsafe "LLVMGetConstOpcode"
  getConstOpcode :: ValueRef -> IO COpcode

foreign import ccall unsafe "LLVMAlignOf"
  alignOf :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMSizeOf"
  sizeOf :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNeg"
  constNeg :: ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstNSWNeg"
  constNSWNeg :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNUWNeg"
  constNUWNeg :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFNeg"
  constFNeg :: ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstNot"
  constNot :: ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstAdd"
  constAdd :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstNSWAdd"
  constNSWAdd :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNUWAdd"
  constNUWAdd :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFAdd"
  constFAdd :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstSub"
  constSub :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstNSWSub"
  constNSWSub :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNUWSub"
  constNUWSub :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFSub"
  constFSub :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstMul"
  constMul :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstNSWMul"
  constNSWMul :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNUWMul"
  constNUWMul :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFMul"
  constFMul :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstUDiv"
  constUDiv :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstSDiv"
  constSDiv :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstExactSDiv"
  constExactSDiv :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFDiv"
  constFDiv :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstURem"
  constURem :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstSRem"
  constSRem :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstFRem"
  constFRem :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstAnd"
  constAnd :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstOr"
  constOr :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstXor"
  constXor :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstICmp"
  constICmp :: CIntPredicate -> ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstFCmp"
  constFCmp :: CRealPredicate -> ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstShl"
  constShl :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstLShr"
  constLShr :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstAShr"
  constAShr :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstGEP"
  constGEP :: ValueRef
           -- ^ Pointer value
           -> Ptr ValueRef
           -- ^ Array of indexes
           -> CUInt
           -- ^ Number of indexes
           -> ValueRef
           -- ^ Constant value

foreign import ccall unsafe "LLVMConstInBoundsGEP"
  constInBoundsGEP :: ValueRef
                   -- ^ Pointer value
                   -> Ptr ValueRef
                   -- ^ Array of indexes
                   -> CUInt
                   -- ^ Number of indexes
                   -> IO ValueRef
                   -- ^ Constant value

foreign import ccall unsafe "LLVMConstTrunc"
  constTrunc :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstSExt"
  constSExt :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstZExt"
  constZExt :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstFPTrunc"
  constFPTrunc :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstFPExt"
  constFPExt :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstUIToFP"
  constUIToFP :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstSIToFP"
  constSIToFP :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstFPToUI"
  constFPToUI :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstFPToSI"
  constFPToSI :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstPtrToInt"
  constPtrToInt :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstIntToPtr"
  constIntToPtr :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstBitCast"
  constBitCast :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstSExtOrBitCast"
  constSExtOrBitCast :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstZExtOrBitCast"
  constZExtOrBitCast :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstTruncOrBitCast"
  constTruncOrBitCast :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstPointerCast"
  constPointerCast :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstIntCast"
  constIntCast :: ValueRef
               -- ^ Value
               -> TypeRef
               -- ^ Result type, must be an integer
               -> CInt
               -- ^ Boolean indicating whether the value is signed
               -> IO ValueRef
               -- ^ Constant value

foreign import ccall unsafe "LLVMConstFPCast"
  constFPCast :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstSelect"
  constSelect :: ValueRef
              -- ^ Test value
              -> ValueRef
              -- ^ True value
              -> ValueRef
              -- ^ False value
              -> ValueRef
              -- ^ Constant value

foreign import ccall unsafe "LLVMConstExtractElement"
  constExtractElement :: ValueRef
                      -- ^ Vector value
                      -> ValueRef
                      -- ^ Index
                      -> ValueRef
                      -- ^ Constant value

foreign import ccall unsafe "LLVMConstInsertElement"
  constInsertElement :: ValueRef
                     -- ^ Vector value
                     -> ValueRef
                     -- ^ Element value
                     -> ValueRef
                     -- ^ Index value
                     -> ValueRef
                     -- ^ Constant value

foreign import ccall unsafe "LLVMConstShuffleVector"
  constShuffleVector :: ValueRef
                     -- ^ First vector
                     -> ValueRef
                     -- ^ Second vector
                     -> ValueRef
                     -- ^ Mask value
                     -> ValueRef

foreign import ccall unsafe "LLVMConstExtractValue"
  constExtractValue :: ValueRef
                    -- ^ Aggregate constant
                    -> Ptr CUInt
                    -- ^ Array of indexes
                    -> CUInt
                    -- ^ Number of indexes
                    -> IO ValueRef
                    -- ^ Constant value

foreign import ccall unsafe "LLVMConstInsertValue"
  constInsertValue :: ValueRef
                   -- ^ Aggregate constant
                   -> ValueRef
                   -- ^ Value
                   -> Ptr CUInt
                   -- ^ Array of indexes
                   -> CUInt
                   -- ^ Number of indexes
                   -> IO ValueRef
                   -- ^ Constant value

foreign import ccall unsafe "LLVMConstInlineAsm"
  constInlineAsm :: TypeRef
                 -- ^ Result type
                 -> CString
                 -- ^ Asm string
                 -> CString
                 -- ^ Constraint string
                 -> CInt
                 -- ^ Boolean value indicating whether or not the
                 -- block has side effects
                 -> CInt
                 -- ^ Boolean value indicating whether or not the
                 -- block preserves stack alignment.
                 -> IO ValueRef
                 -- ^ Constant value

foreign import ccall unsafe "LLVMBlockAddress"
  blockAddress :: ValueRef -> BasicBlockRef -> IO ValueRef

-- * Operations on globals

foreign import ccall unsafe "LLVMGetGlobalParent"
  getGlobalParent :: ValueRef -> IO ModuleRef

foreign import ccall unsafe "LLVMIsDeclaration"
  isDeclaration :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMGetLinkage"
  getLinkage :: ValueRef -> IO CLinkage

foreign import ccall unsafe "LLVMSetLinkage"
  setLinkage :: ValueRef -> CLinkage -> IO ()

foreign import ccall unsafe "LLVMGetSection"
  getSection :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetSection"
  setSection :: ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMGetVisibility"
  getVisibility :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetVisibility"
  setVisibility :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetAlignment"
  getAlignment :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetAlignment"
  setAlignment :: ValueRef -> CUInt -> IO ()

-- * Global variables

foreign import ccall unsafe "LLVMAddGlobal"
  addGlobal :: ModuleRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMAddGlobalInAddressSpace"
  addGlobalInAddressSpace :: ModuleRef -> TypeRef -> CString -> CUInt ->
                             IO ValueRef

foreign import ccall unsafe "LLVMGetNamedGlobal"
  getNamedGlobal :: ModuleRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMGetFirstGlobal"
  getFirstGlobal :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastGlobal"
  getLastGlobal :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNextGlobal"
  getNextGlobal :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousGlobal"
  getPreviousGlobal :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMDeleteGlobal"
  deleteGlobal :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMSetInitializer"
  setInitializer :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetInitializer"
  getInitializer :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMIsThreadLocal"
  isThreadLocal :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMSetThreadLocal"
  setThreadLocal :: ValueRef -> CInt -> IO ()

foreign import ccall unsafe "LLVMIsGlobalConstant"
  isGlobalConstant :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMSetGlobalConstant"
  setGlobalConstant :: ValueRef -> CInt -> IO ()

-- ** Aliases
foreign import ccall unsafe "LLVMAddAlias"
  addAlias :: ModuleRef
           -- ^ Module
           -> TypeRef
           -- ^ Type
           -> ValueRef
           -- ^ Aliasee
           -> CString
           -- ^ Name
           -> IO ValueRef
           -- ^ Alias value

-- * Functions

-- | Remove a function from its containing module and deletes it.
foreign import ccall unsafe "LLVMDeleteFunction"
  deleteFunction :: ValueRef -> IO ()

-- | Obtain the ID number from a function instance.
foreign import ccall unsafe "LLVMGetIntrinsicID"
  getIntrinsicID :: ValueRef -> CUInt

-- | Obtain the calling function of a function.
--
-- The returned value corresponds to the LLVMCallConv enumeration.
foreign import ccall unsafe "LLVMGetFunctionCallConv"
  getFunctionCallConv :: ValueRef -> IO CUInt

-- | Set the calling convention of a function.
foreign import ccall unsafe "LLVMSetFunctionCallConv"
  setFunctionCallConv :: ValueRef -> CUInt -> IO ()

-- | Obtain the name of the garbage collector to use during code
-- generation.
foreign import ccall unsafe "LLVMGetGC"
  getGC :: ValueRef -> IO CString

-- | Define the garbage collector to use during code generation.
foreign import ccall unsafe "LLVMSetGC"
  setGC :: ValueRef -> CString -> IO ()

-- | Add an attribute to a function.
foreign import ccall unsafe "LLVMAddFunctionAttr"
  addFunctionAttr :: ValueRef -> CAttribute -> IO ()

-- | Obtain attributes for a function.
foreign import ccall unsafe "LLVMGetFunctionAttr"
  getFunctionAttr :: ValueRef -> IO CUInt

-- | Remove an attribute from a function.
foreign import ccall unsafe "LLVMRemoveFunctionAttr"
  removeFunctionAttr :: ValueRef -> CAttribute -> IO ()

-- Parameters

-- | Obtain the number of parameters in a function.
foreign import ccall unsafe "LLVMCountParams"
  countParams :: ValueRef -> CUInt

-- | Obtain the parameters in a function.
foreign import ccall unsafe "LLVMGetParams"
  getParams :: ValueRef
            -- ^ Function
            -> Ptr ValueRef
            -- ^ Array at least as long as number of parameters to
            -- fill
            -> IO ()

-- | Obtain the parameter at the specified index.
--
-- Parameters are indexed from 0.
foreign import ccall unsafe "LLVMGetParam"
  getParam :: ValueRef
           -- ^ Function
           -> CUInt
           -- ^ Parameter index
           -> ValueRef
           -- ^ Parameter value

-- | Obtain the function to which this argument belongs.
--
-- Unlike other functions in this group, this one takes a LLVMValueRef
-- that corresponds to a llvm::Attribute.
--
-- The returned LLVMValueRef is the llvm::Function to which this
-- argument belongs.
foreign import ccall unsafe "LLVMGetParamParent"
  getParamParent :: ValueRef -> IO ValueRef

-- | Obtain the first parameter to a function.
foreign import ccall unsafe "LLVMGetFirstParam"
  getFirstParam :: ValueRef -> IO ValueRef

-- | Obtain the last parameter to a function.
foreign import ccall unsafe "LLVMGetLastParam"
  getLastParam :: ValueRef -> IO ValueRef

-- | Obtain the next parameter to a function.
--
-- This takes a ValueRef obtained from getetFirstParam (which is
-- actually a wrapped iterator) and obtains the next parameter from the
-- underlying iterator.
foreign import ccall unsafe "LLVMGetNextParam"
  getNextParam :: ValueRef -> IO ValueRef

-- | Obtain the previous parameter to a function.
--
-- This is the opposite of getNextParam.
foreign import ccall unsafe "LLVMGetPreviousParam"
  getPreviousParam :: ValueRef -> IO ValueRef

-- | Add an attribute to a function argument.
foreign import ccall unsafe "LLVMAddAttribute"
  addAttribute :: ValueRef -> CAttribute -> IO ()

-- | Remove an attribute from a function argument.
foreign import ccall unsafe "LLVMRemoveAttribute"
  removeAttribute :: ValueRef -> CAttribute -> IO ()

-- | Get the attributes of a function argument.
foreign import ccall unsafe "LLVMGetAttribute"
  getAttribute :: ValueRef -> IO CUInt

-- | Set the alignment for a function parameter.
foreign import ccall unsafe "LLVMSetParamAlignment"
  setParamAlignment :: ValueRef -> CUInt -> IO ()

-- ** Metadata

-- | Obtain a MDString value from a context.
--
-- The returned instance corresponds to the llvm::MDString class.
--
-- The instance is specified by string data of a specified length. The
-- string content is copied, so the backing memory can be freed after
-- this function returns.
foreign import ccall unsafe "LLVMMDStringInContext"
  mdStringInContext :: ContextRef
                    -- ^ Context
                    -> CString
                    -- ^ String
                    -> CUInt
                    -- ^ Length of string
                    -> IO ValueRef
                    -- ^ Metadata node value

-- | Obtain a MDString value from the global context.
foreign import ccall unsafe "LLVMMDString"
  mdString :: CString
           -- ^ String
           -> CUInt
           -- ^ Length of string
           -> IO ValueRef
           -- ^ Metadata node value

-- | Obtain a MDNode value from a context.
--
-- The returned value corresponds to the llvm::MDNode class.
foreign import ccall unsafe "LLVMMDNodeInContext"
  mdNodeInContext :: ContextRef
                  -- ^ Context
                  -> Ptr ValueRef
                  -- ^ Array of values
                  -> CUInt
                  -- ^ Number of values
                  -> IO ValueRef
                  -- ^ Metadata node value

-- | Obtain a MDNode value from the global context.
foreign import ccall unsafe "LLVMMDNode"
  mdNode :: Ptr ValueRef
         -- ^ Array of values
         -> CUInt
         -- ^ Number of values
         -> IO ValueRef
         -- ^ Metadata node value

-- | Obtain the underlying string from a MDString value.
foreign import ccall unsafe "LLVMGetMDString"
  getMDString :: ValueRef
              -- ^ Metadata node value
              -> Ptr CUInt
              -- ^ Stores length of string
              -> IO CString
              -- ^ String value

-- ** Basic Blocks

-- | Convert a basic block instance to a value type.
foreign import ccall unsafe "LLVMBasicBlockAsValue"
  basicBlockAsValue :: BasicBlockRef -> ValueRef

-- | Determine whether a LLVMValueRef is itself a basic block.
foreign import ccall unsafe "LLVMValueIsBasicBlock"
  valueIsBasicBlock :: ValueRef -> CInt

-- | Convert a LLVMValueRef to a LLVMBasicBlockRef instance.
foreign import ccall unsafe "LLVMValueAsBasicBlock"
  valueAsBasicBlock :: ValueRef -> BasicBlockRef

-- | Obtain the function to which a basic block belongs.
foreign import ccall unsafe "LLVMGetBasicBlockParent"
  getBasicBlockParent :: BasicBlockRef -> IO ValueRef

-- | Obtain the terminator instruction for a basic block.
--
-- If the basic block does not have a terminator (it is not well-formed
-- if it doesn't), then NULL is returned.
--
-- The returned LLVMValueRef corresponds to a llvm::TerminatorInst.
foreign import ccall unsafe "LLVMGetBasicBlockTerminator"
  getBasicBlockTerminator :: BasicBlockRef -> IO ValueRef

-- | Obtain the number of basic blocks in a function.
foreign import ccall unsafe "LLVMCountBasicBlocks"
  countBasicBlocks :: ValueRef -> IO CUInt

-- | Obtain all of the basic blocks in a function.
foreign import ccall unsafe "LLVMGetBasicBlocks"
  getBasicBlocks :: ValueRef
                 -- ^ Function value
                 -> Ptr BasicBlockRef
                 -- ^ Array at least as long as the number of basic
                 -- blocks.
                 -> IO ()

-- | Obtain the first basic block in a function.
--
-- The returned basic block can be used as an iterator. You will likely
-- eventually call into getNextBasicBlock with it.
foreign import ccall unsafe "LLVMGetFirstBasicBlock"
  getFirstBasicBlock :: ValueRef -> IO BasicBlockRef

-- | Obtain the last basic block in a function.
foreign import ccall unsafe "LLVMGetLastBasicBlock"
  getLastBasicBlock :: ValueRef -> IO BasicBlockRef

-- | Advance a basic block iterator.
foreign import ccall unsafe "LLVMGetNextBasicBlock"
  getNextBasicBlock :: BasicBlockRef -> IO BasicBlockRef

-- | Go backwards in a basic block iterator.
foreign import ccall unsafe "LLVMGetPreviousBasicBlock"
  getPreviousBasicBlock :: BasicBlockRef -> IO BasicBlockRef

-- | Obtain the basic block that corresponds to the entry point of a
-- function.
foreign import ccall unsafe "LLVMGetEntryBasicBlock"
  getEntryBasicBlock :: ValueRef -> IO BasicBlockRef

-- | Append a basic block to the end of a function.
foreign import ccall unsafe "LLVMAppendBasicBlockInContext"
  appendBasicBlockInContext :: ContextRef -> ValueRef -> CString ->
                               IO BasicBlockRef

-- | Insert a basic block in a function before another basic block.
--
-- The function to add to is determined by the function of the
-- passed basic block.
foreign import ccall unsafe "LLVMInsertBasicBlockInContext"
  insertBasicBlockInContext :: ContextRef -> BasicBlockRef -> CString ->
                               IO BasicBlockRef

-- | Insert a basic block in a function using the global context.
foreign import ccall unsafe "LLVMAppendBasicBlock"
  appendBasicBlock :: ValueRef -> CString -> IO BasicBlockRef

-- | Insert a basic block in a function using the global context.
foreign import ccall unsafe "LLVMInsertBasicBlock"
  insertBasicBlock :: BasicBlockRef -> CString -> IO BasicBlockRef

-- | Remove a basic block from a function and delete it.
--
-- This deletes the basic block from its containing function and deletes
-- the basic block itself.
foreign import ccall unsafe "LLVMDeleteBasicBlock"
  deleteBasicBlock :: BasicBlockRef -> IO ()

-- | Remove a basic block from a function.
--
-- This deletes the basic block from its containing function but keep
-- the basic block alive.
foreign import ccall unsafe "LLVMRemoveBasicBlockFromParent"
  removeBasicBlockFromParent :: BasicBlockRef -> IO ()

-- | Move a basic block to before another one.
foreign import ccall unsafe "LLVMMoveBasicBlockBefore"
  moveBasicBlockBefore :: BasicBlockRef
                       -- ^ Block
                       -> BasicBlockRef
                       -- ^ Position
                       -> IO ()

-- | Move a basic block to after another one.
foreign import ccall unsafe "LLVMMoveBasicBlockAfter"
  moveBasicBlockAfter :: BasicBlockRef
                      -- ^ Block
                      -> BasicBlockRef
                      -- ^ Position
                      -> IO ()

-- | Obtain the first instruction in a basic block.
--
-- The returned LLVMValueRef corresponds to a llvm::Instruction
-- instance.
foreign import ccall unsafe "LLVMGetFirstInstruction"
  getFirstInstruction :: BasicBlockRef -> IO ValueRef

-- | Obtain the last instruction in a basic block.
--
-- The returned LLVMValueRef corresponds to a LLVM:Instruction.
foreign import ccall unsafe "LLVMGetLastInstruction"
  getLastInstruction :: BasicBlockRef -> IO ValueRef

-- Instructions

-- | Determine whether an instruction has any metadata attached.
foreign import ccall unsafe "LLVMHasMetadata"
  hasMetadata :: ValueRef -> IO CInt

-- | Return metadata associated with an instruction value.
foreign import ccall unsafe "LLVMGetMetadata"
  getMetadata :: ValueRef -> CUInt -> IO ValueRef

-- | Set metadata associated with an instruction value.
foreign import ccall unsafe "LLVMSetMetadata"
  setMetadata :: ValueRef -> CUInt -> ValueRef -> IO ()

-- | Obtain the basic block to which an instruction belongs.
foreign import ccall unsafe "LLVMGetInstructionParent"
  getInstructionParent :: ValueRef -> IO BasicBlockRef

-- | Obtain the instruction that occurs after the one specified.
--
-- The next instruction will be from the same basic block.
--
-- If this is the last instruction in a basic block, NULL will be
-- returned.
foreign import ccall unsafe "LLVMGetNextInstruction"
  getNextInstruction :: ValueRef -> IO ValueRef

-- | Obtain the instruction that occured before this one.
--
-- If the instruction is the first instruction in a basic block, NULL
-- will be returned.
foreign import ccall unsafe "LLVMGetPreviousInstruction"
  getPreviousInstruction :: ValueRef -> IO ValueRef

-- | Remove and delete an instruction.
--
-- The instruction specified is removed from its containing building
-- block and then deleted.
foreign import ccall unsafe "LLVMInstructionEraseFromParent"
  instructionEraseFromParent :: ValueRef -> IO ()

-- | Obtain the code opcode for an individual instruction.
foreign import ccall unsafe "LLVMGetInstructionOpcode"
  getInstructionOpcode :: ValueRef -> IO COpcode

-- | Obtain the predicate of an instruction.
--
-- This is only valid for instructions that correspond to llvm::ICmpInst
-- or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
foreign import ccall unsafe "LLVMGetICmpPredicate"
  getICmpPredicate :: ValueRef -> IO CIntPredicate

-- Call sites

-- | Set the calling convention for a call instruction.
--
-- This expects an LLVMValueRef that corresponds to a llvm::CallInst or
-- llvm::InvokeInst.
foreign import ccall unsafe "LLVMSetInstructionCallConv"
  setInstructionCallConv :: ValueRef -> CUInt -> IO ()

-- | Obtain the calling convention for a call instruction.
foreign import ccall unsafe "LLVMGetInstructionCallConv"
  getInstructionCallConv :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMAddInstrAttribute"
  addInstrAttribute :: ValueRef
                    -- ^ Instruction
                    -> CUInt
                    -- ^ Argument Index
                    -> CAttribute
                    -- ^ Attribute
                    -> IO ()

foreign import ccall unsafe "LLVMRemoveInstrAttribute"
  removeInstrAttribute :: ValueRef
                       -- ^ Instruction
                       -> CUInt
                       -- ^ Argument Index
                       -> CAttribute
                       -- ^ Attribute
                       -> IO ()

foreign import ccall unsafe "LLVMSetInstrParamAlignment"
  setInstrParamAlignment :: ValueRef
                         -- ^ Instruction
                         -> CUInt
                         -- ^ Index
                         -> CUInt
                         -- ^ Alignment
                         -> IO ()

-- Call instructions

-- | Obtain whether a call instruction is a tail call.
--
-- This only works on llvm::CallInst instructions.
foreign import ccall unsafe "LLVMIsTailCall"
  isTailCall :: ValueRef -> IO CInt

-- | Set whether a call instruction is a tail call.
--
-- This only works on llvm::CallInst instructions.
foreign import ccall unsafe "LLVMSetTailCall"
  setTailCall :: ValueRef -> CInt -> IO ()

-- Switch Instructions
-- | Obtain the default destination basic block of a switch instruction.
--
-- This only works on llvm::SwitchInst instructions.
foreign import ccall unsafe "LLVMGetSwitchDefaultDest"
  getSwitchDefaultDest :: ValueRef -> IO BasicBlockRef

-- ** Phi Nodes
-- | Add incoming values to the end of a PHI list.
foreign import ccall unsafe "LLVMAddIncoming"
  addIncoming :: ValueRef
              -- ^ Phi instruction
              -> Ptr ValueRef
              -- ^ Array of values
              -> Ptr ValueRef
              -- ^ Array of basic blocks
              -> CUInt
              -- ^ Number of incoming values
              -> IO ()

-- | Obtain the number of incoming basic blocks to a PHI node.
foreign import ccall unsafe "LLVMCountIncoming"
  countIncoming :: ValueRef -> IO CUInt

-- | Obtain an incoming value to a PHI node as a LLVMValueRef.
foreign import ccall unsafe "LLVMGetIncomingValue"
  getIncomingValue :: ValueRef -> CUInt -> IO ValueRef

-- | Obtain an incoming value to a PHI node as a LLVMBasicBlockRef.
foreign import ccall unsafe "LLVMGetIncomingBlock"
  getIncomingBlock :: ValueRef -> CUInt -> IO BasicBlockRef

-- Builders

foreign import ccall unsafe "LLVMCreateBuilderInContext"
  createBuilderInContext :: ContextRef -> IO BuilderRef

foreign import ccall unsafe "LLVMCreateBuilder"
  createBuilder :: IO BuilderRef

foreign import ccall unsafe "LLVMPositionBuilder"
  positionBuilder :: BuilderRef -> BasicBlockRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderBefore"
  positionBefore :: BuilderRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderAtEnd"
  positionAtEnd :: BuilderRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMGetInsertBlock"
  getInsertBlock :: BuilderRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMClearInsertionPosition"
  clearInsertionPosition :: BuilderRef -> IO ()

foreign import ccall unsafe "LLVMInsertIntoBuilder"
  insertIntoBuilder :: BuilderRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMInsertIntoBuilderWithName"
  insertIntoBuilderWithName :: BuilderRef -> ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMDisposeBuilder"
  disposeBuilder :: BuilderRef -> IO ()

foreign import ccall unsafe "&LLVMDisposeBuilder"
  ptrDisposeBuilder :: FunPtr (BuilderRef -> IO ())

-- Metadata
foreign import ccall unsafe "LLVMGetCurrentDebugLocation"
  getCurrentDebugLocation :: BuilderRef -> IO ValueRef

foreign import ccall unsafe "LLVMSetCurrentDebugLocation"
  setCurrentDebugLocation :: BuilderRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMSetInstDebugLocation"
  setInstDebugLocation :: BuilderRef -> ValueRef -> IO ()

-- Terminators
foreign import ccall unsafe "LLVMBuildRetVoid"
  buildRetVoid :: BuilderRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildRet"
  buildRet :: BuilderRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAggregateRet"
  buildAggregateRet :: BuilderRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMBuildBr"
  buildBr :: BuilderRef -> BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCondBr"
  buildCondBr :: BuilderRef -> ValueRef -> BasicBlockRef -> BasicBlockRef ->
                 IO ValueRef

foreign import ccall unsafe "LLVMBuildSwitch"
  buildSwitch :: BuilderRef -> ValueRef -> BasicBlockRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMBuildIndirectBr"
  buildIndirectBr :: BuilderRef -> ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMBuildInvoke"
  buildInvoke :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt ->
                 BasicBlockRef -> BasicBlockRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildLandingPad"
  buildLandingPad :: BuilderRef -> TypeRef -> ValueRef -> CUInt -> CString ->
                     IO ValueRef

foreign import ccall unsafe "LLVMBuildResume"
  buildResume :: BuilderRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildUnreachable"
  buildUnreachable :: BuilderRef -> IO ValueRef

-- ** Switch instructions
foreign import ccall unsafe "LLVMAddCase"
  addCase :: ValueRef -> ValueRef -> BasicBlockRef -> IO ()

-- ** IndirectBr instructions
foreign import ccall unsafe "LLVMAddDestination"
  addDestination :: ValueRef -> BasicBlockRef -> IO ()

-- ** LandingPad instructions
foreign import ccall unsafe "LLVMAddClause"
  addClause :: ValueRef -> ValueRef -> IO ()

-- ** Resume instructions
foreign import ccall unsafe "LLVMSetCleanup"
  setCleanup :: ValueRef -> CInt -> IO ()

-- ** Arithmetic
foreign import ccall unsafe "LLVMBuildAdd"
  buildAdd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNSWAdd"
  buildNSWAdd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNUWAdd"
  buildNUWAdd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFAdd"
  buildFAdd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSub"
  buildSub :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNSWSub"
  buildNSWSub :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNUWSub"
  buildNUWSub :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFSub"
  buildFSub :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildMul"
  buildMul :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNSWMul"
  buildNSWMul :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNUWMul"
  buildNUWMul :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFMul"
  buildFMul :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildUDiv"
  buildUDiv :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSDiv"
  buildSDiv :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildExactSDiv"
  buildExactSDiv :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFDiv"
  buildFDiv :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildURem"
  buildURem :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSRem"
  buildSRem :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFRem"
  buildFRem :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildShl"
  buildShl :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildLShr"
  buildLShr :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAShr"
  buildAShr :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAnd"
  buildAnd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildOr"
  buildOr :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildXor"
  buildXor :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildBinOp"
  buildBinOp :: BuilderRef -> COpcode -> ValueRef -> ValueRef -> CString ->
                IO ValueRef

foreign import ccall unsafe "LLVMBuildNeg"
  buildNeg :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNSWNeg"
  buildNSWNeg :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNUWNeg"
  buildNUWNeg :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFNeg"
  buildFNeg :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNot"
  buildNot :: BuilderRef -> ValueRef -> CString -> IO ValueRef

-- ** Memory
foreign import ccall unsafe "LLVMBuildMalloc"
  buildMalloc :: BuilderRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildArrayMalloc"
  buildArrayMalloc :: BuilderRef -> TypeRef -> ValueRef -> CString ->
                      IO ValueRef

foreign import ccall unsafe "LLVMBuildAlloca"
  buildAlloca :: BuilderRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildArrayAlloca"
  buildArrayAlloca :: BuilderRef -> TypeRef -> ValueRef -> CString ->
                      IO ValueRef

foreign import ccall unsafe "LLVMBuildFree"
  buildFree :: BuilderRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildLoad"
  buildLoad :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildStore"
  buildStore :: BuilderRef
             -- ^ Builder
             -> ValueRef
             -- ^ Value
             -> ValueRef
             -- ^ Pointer value
             -> IO ValueRef
             -- ^ Store instruction

foreign import ccall unsafe "LLVMBuildGEP"
  buildGEP :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString ->
              IO ValueRef

foreign import ccall unsafe "LLVMBuildInBoundsGEP"
  buildInBoundsGEP :: BuilderRef -> ValueRef -> (Ptr ValueRef) -> CUInt ->
                      CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildStructGEP"
  buildStructGEP :: BuilderRef -> ValueRef -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildGlobalString"
  buildGlobalString :: BuilderRef -> CString -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildGlobalStringPtr"
  buildGlobalStringPtr :: BuilderRef -> CString -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMGetVolatile"
  getVolatile :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMSetVolatile"
  setVolatile :: ValueRef -> CInt -> IO ()

-- Casts
foreign import ccall unsafe "LLVMBuildTrunc"
  buildTrunc :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildZExt"
  buildZExt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSExt"
  buildSExt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPToUI"
  buildFPToUI :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPToSI"
  buildFPToSI :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildUIToFP"
  buildUIToFP :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSIToFP"
  buildSIToFP :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPTrunc"
  buildFPTrunc :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPExt"
  buildFPExt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildPtrToInt"
  buildPtrToInt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildIntToPtr"
  buildIntToPtr :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildBitCast"
  buildBitCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildZExtOrBitCast"
  buildZExtOrBitCast :: BuilderRef -> ValueRef -> TypeRef -> CString ->
                        IO ValueRef

foreign import ccall unsafe "LLVMBuildSExtOrBitCast"
  buildSExtOrBitCast :: BuilderRef -> ValueRef -> TypeRef -> CString ->
                        IO ValueRef

foreign import ccall unsafe "LLVMBuildTruncOrBitCast"
  buildTruncOrBitCast :: BuilderRef -> ValueRef -> TypeRef -> CString ->
                         IO ValueRef

foreign import ccall unsafe "LLVMBuildCast"
  buildCast :: BuilderRef -> COpcode -> ValueRef -> TypeRef -> CString ->
               IO ValueRef

foreign import ccall unsafe "LLVMBuildPointerCast"
  buildPointerCast :: BuilderRef -> ValueRef -> TypeRef -> CString ->
                      IO ValueRef

foreign import ccall unsafe "LLVMBuildIntCast"
  buildIntCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPCast"
  buildFPCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

-- Comparisons
foreign import ccall unsafe "LLVMBuildICmp"
  buildICmp :: BuilderRef -> CUInt -> ValueRef -> ValueRef -> CString ->
               IO ValueRef

foreign import ccall unsafe "LLVMBuildFCmp"
  buildFCmp :: BuilderRef -> CUInt -> ValueRef -> ValueRef -> CString ->
               IO ValueRef

-- Miscellaneous instructions
foreign import ccall unsafe "LLVMBuildPhi"
  buildPhi :: BuilderRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCall"
  buildCall :: BuilderRef
            -- ^ Builder
            -> ValueRef
            -- ^ Function value
            -> Ptr ValueRef
            -- ^ Array of arguments
            -> CUInt
            -- ^ Number of arguments
            -> CString
            -- ^ Name
            -> IO ValueRef
            -- ^ Intruction

foreign import ccall unsafe "LLVMBuildSelect"
  buildSelect :: BuilderRef
              -- ^ Builder
              -> ValueRef
              -- ^ Test
              -> ValueRef
              -- ^ True case
              -> ValueRef
              -- ^ False case
              -> CString
              -- ^ Name
              -> IO ValueRef
              -- ^ Instruction

foreign import ccall unsafe "LLVMBuildVAArg"
  buildVAArg :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildExtractElement"
  buildExtractElement :: BuilderRef
                      -- ^ Builder
                      -> ValueRef
                      -- ^ Vector value
                      -> ValueRef
                      -- ^ Index value
                      -> CString
                      -- ^ Name
                      -> IO ValueRef
                      -- ^ Instruction

foreign import ccall unsafe "LLVMBuildInsertElement"
  buildInsertElement :: BuilderRef
                     -- ^ Builder
                     -> ValueRef
                     -- ^ Vector value
                     -> ValueRef
                     -- ^ Index value
                     -> ValueRef
                     -- ^ Element value
                     -> CString
                     -- ^ Name
                     -> IO ValueRef
                     -- ^ Instruction

foreign import ccall unsafe "LLVMBuildShuffleVector"
  buildShuffleVector :: BuilderRef -> ValueRef -> ValueRef -> ValueRef ->
                        CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildExtractValue"
  buildExtractValue :: BuilderRef
                    -- ^ Builder
                    -> ValueRef
                    -- ^ Aggregate value
                    -> CUInt
                    -- ^ Index
                    -> CString
                    -- ^ Name
                    -> IO ValueRef
                    -- ^ Instruction

foreign import ccall unsafe "LLVMBuildInsertValue"
  buildInsertValue :: BuilderRef
                   -- ^ Builder
                   -> ValueRef
                   -- ^ Aggregate value
                   -> ValueRef
                   -- ^ Element value
                   -> CUInt
                   -- ^ Index
                   -> CString
                   -- ^ Name
                   -> IO ValueRef
                   -- ^ Instruction

foreign import ccall unsafe "LLVMBuildIsNull"
  buildIsNull :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildIsNotNull"
  buildIsNotNull :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildPtrDiff"
  buildPtrDiff :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

-- Module Providers
-- | Changes the type of M so it can be passed to FunctionPassManagers and the
-- JIT.  They take ModuleProviders for historical reasons.
foreign import ccall unsafe "LLVMCreateModuleProviderForExistingModule"
  createModuleProviderForExistingModule :: ModuleRef -> IO ModuleProviderRef

-- | Destroys the module M.
foreign import ccall unsafe "LLVMDisposeModuleProvider"
  disposeModuleProvider :: ModuleProviderRef -> IO ()

foreign import ccall unsafe "&LLVMDisposeModuleProvider"
  ptrDisposeModuleProvider :: FunPtr (ModuleProviderRef -> IO ())

-- Memory Buffers
foreign import ccall unsafe "LLVMCreateMemoryBufferWithContentsOfFile"
  createMemoryBufferWithContentsOfFile :: CString -> Ptr MemoryBufferRef ->
                                          Ptr CString -> IO CInt

foreign import ccall unsafe "LLVMCreateMemoryBufferWithSTDIN"
  createMemoryBufferWithSTDIN :: Ptr MemoryBufferRef -> Ptr CString -> IO CInt

foreign import ccall unsafe "LLVMDisposeMemoryBuffer"
  disposeMemoryBuffer :: MemoryBufferRef -> IO ()

-- Pass Registry

-- | Return the global pass registry, for use with initialization functions.
foreign import ccall unsafe "LLVMGetGlobalPassRegistry"
  getGlobalPassRegistry :: IO PassRegistryRef

-- ** Pass Managers

-- | Constructs a new whole-module pass pipeline. This type of
-- pipeline is suitable for link-time optimization and whole-module
-- transformations.
foreign import ccall unsafe "LLVMCreatePassManager"
  createPassManager :: IO PassManagerRef

-- | Constructs a new function-by-function pass pipeline over the
-- module provider. It does not take ownership of the module
-- provider. This type of pipeline is suitable for code generation and
-- JIT compilation tasks.
foreign import ccall unsafe "LLVMCreateFunctionPassManagerForModule"
  createFunctionPassManagerForModule :: ModuleRef -> IO PassManagerRef

-- | Deprecated: Use LLVMCreateFunctionPassManagerForModule instead.
foreign import ccall unsafe "LLVMCreateFunctionPassManager"
  createFunctionPassManager :: ModuleProviderRef -> IO PassManagerRef

-- | Initializes, executes on the provided module, and finalizes all
-- of the passes scheduled in the pass manager. Returns 1 if any of
-- the passes modified the module, 0 otherwise.
foreign import ccall unsafe "LLVMRunPassManager"
  runPassManager :: PassManagerRef -> ModuleRef -> IO CInt

-- | Initializes all of the function passes scheduled in the function
-- pass manager. Returns 1 if any of the passes modified the module, 0
-- otherwise.
foreign import ccall unsafe "LLVMInitializeFunctionPassManager"
  initializeFunctionPassManager :: PassManagerRef -> IO CInt

-- | Executes all of the function passes scheduled in the function
-- pass manager on the provided function. Returns 1 if any of the
-- passes modified the function, false otherwise.
foreign import ccall unsafe "LLVMRunFunctionPassManager"
  runFunctionPassManager :: PassManagerRef -> ValueRef -> IO CInt

-- | Finalizes all of the function passes scheduled in in the function
-- pass manager. Returns 1 if any of the passes modified the module, 0
-- otherwise.
foreign import ccall unsafe "LLVMFinalizeFunctionPassManager"
  finalizeFunctionPassManager :: PassManagerRef -> IO CInt

-- | Frees the memory of a pass pipeline. For function pipelines, does
-- not free the module provider.
foreign import ccall unsafe "LLVMDisposePassManager"
  disposePassManager :: PassManagerRef -> IO ()

foreign import ccall unsafe "&LLVMDisposePassManager"
  ptrDisposePassManager :: FunPtr (PassManagerRef -> IO ())
