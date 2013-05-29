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

-- | Top-level LLVM module.  Exports Analysis, BitReader/Writer, and
-- Core, with the monadic versions of Context, Module, and Builder
-- functions taking precedence.
module LLVM(
       -- * Analysis Module
       verifyFunction,
       viewFunctionCFG,
       viewFunctionCFGOnly,

       -- * BitReader Module
       parseBitcode,
       getBitcodeModule,

       -- * Core Module
       -- ** Types
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

       -- ** Error handling
       disposeMessage,

       -- ** Context functions.
       contextCreate,
       contextDispose,
       getGlobalContext,
       getMDKindID,

       -- ** Modules
       moduleCreateWithName,

       -- *** Functions
       getNextFunction,
       getPreviousFunction,

       -- ** Types
       getTypeKind,
       typeIsSized,
       getTypeContext,

       -- *** Integer types
       int1Type,
       int8Type,
       int16Type,
       int32Type,
       int64Type,
       intType,
       getIntTypeWidth,

       -- *** Floating point types
       floatType,
       doubleType,
       x86FP80Type,
       fp128Type,
       ppcFP128Type,

       -- *** Function types
       functionType,
       isFunctionVarArg,
       getReturnType,
       countParamTypes,
       getParamTypes,

       -- *** Struct types
       structType,
       getStructName,
       structSetBody,
       countStructElementTypes,
       getStructElementTypes,
       isPackedStruct,
       isOpaqueStruct,

       -- *** Array, pointer, and vector types
       arrayType,
       pointerType,
       vectorType,
       getElementType,
       getArrayLength,
       getPointerAddressSpace,
       getVectorSize,

       -- *** Other types
       voidType,
       labelType,
       x86MMXType,

       -- ** Values
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

       -- *** Uses
       getFirstUse,
       getNextUse,
       getUser,
       getUsedValue,

       -- *** Users
       getOperand,
       setOperand,
       getNumOperands,

       -- ** Constants
       constNull,
       constAllOnes,
       getUndef,
       isConstant,
       isNull,
       isUndef,
       constPointerNull,

       -- *** Scalar constants
       constInt,
       constIntOfArbitraryPrecision,
       constIntOfString,
       constReal,
       constRealOfString,
       constIntGetZExtValue,
       constIntGetSExtValue,

       -- *** Composite constants
       constString,
       constArray,
       constStruct,
       constNamedStruct,
       constVector,

       -- *** Constant Expressions
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

       -- ** Global variables, functions, and aliases (globals)
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

       -- *** Global variables
       getNextGlobal,
       getPreviousGlobal,
       deleteGlobal,
       getInitializer,
       setInitializer,
       isThreadLocal,
       setThreadLocal,
       isGlobalConstant,
       setGlobalConstant,

       -- *** Functions
       deleteFunction,
       getIntrinsicID,
       getFunctionCallConv,
       setFunctionCallConv,
       getGC,
       setGC,
       addFunctionAttr,
       removeFunctionAttr,

       -- *** Parameters
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

       -- *** Metadata
       mdString,
       mdNode,
       getMDString,

       -- *** Basic blocks
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
       appendBasicBlock,
       insertBasicBlock,
       deleteBasicBlock,
       removeBasicBlockFromParent,
       moveBasicBlockBefore,
       moveBasicBlockAfter,
       getFirstInstruction,
       getLastInstruction,

       -- *** Instructions
       getInstructionParent,
       getNextInstruction,
       getPreviousInstruction,
       instructionEraseFromParent,
       getInstructionOpcode,
       getICmpPredicate,

       -- *** Call Sites
       getInstructionCallConv,
       setInstructionCallConv,
       addInstrAttribute,
       removeInstrAttribute,
       setInstrParamAlignment,

       -- *** Call Instructions (only)
       isTailCall,
       setTailCall,

       -- *** Switch Instructions (only)
       getSwitchDefaultDest,

       -- *** Phi nodes
       addIncoming,
       countIncoming,
       getIncomingValue,
       getIncomingBlock,

       -- ** Instruction building
       createBuilder,
       disposeBuilder,

       -- ** Memory buffers
       createMemoryBufferWithContentsOfFile,
       createMemoryBufferWithSTDIN,
       disposeMemoryBuffer,

       -- ** PassRegistry
       getGlobalPassRegistry,

       -- ** Pass manager
       createPassManager,
       createFunctionPassManagerForModule,
       createFunctionPassManager,
       runPassManager,
       initializeFunctionPassManager,
       runFunctionPassManager,
       finalizeFunctionPassManager,
       disposePassManager,

       -- * Metadata Module
       -- ** TBAA Metadata
       tbaaRootMetadata,
       tbaaMetadata,

       -- ** FP Math Metadata
       fpMathMetadata,

       -- ** Loop Metadata
       loopMetadata,

       -- ** Debug Metadata
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

       -- * Monads
       -- ** Context Monad
       MonadLLVMContext(..),
       LLVMContextT,
       LLVMContext,
       runWithContext,
       runWithNewContext,
       runWithGlobalContext,

       -- ** Module Monad
       MonadLLVMModule(..),
       LLVMModuleT,
       LLVMModule,
       runWithModule,
       runWithNewModule,
       runWithNewModuleInContext,

       -- ** Builder Monad
       MonadLLVMBuilder(..),
       LLVMBuilderT,
       LLVMBuilder,
       runWithBuilder,
       runWithNewBuilder,
       runWithNewBuilderInContext
       ) where

import Control.Monad.LLVM.LLVMContext
import Control.Monad.LLVM.LLVMModule
import Control.Monad.LLVM.LLVMBuilder
import LLVM.Analysis
import LLVM.BitReader
import LLVM.Core
import LLVM.Metadata