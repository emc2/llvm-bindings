{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

-- |
-- Module:      LLVM.FFI.Core
-- Copyright:   Eric McCorkle
-- License:     BSD
--
-- Maintainer:  eric@shadowsun.net
-- Stability:   experimental
-- Portability: requires GHC 7.0.4, LLVM-3.1
--
-- This module provides direct access to the LLVM C bindings.

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
       contextCreate,
       contextDispose,
       getGlobalContext,
       getMDKindID,
       getMDKindIDInContext,

       -- * Modules
       moduleCreateWithName,
       moduleCreateWithNameInContext,
       disposeModule,
       ptrDisposeModule,
       getDataLayout,
       setDataLayout,
       getTarget,
       setTarget,
       dumpModule,
       setModuleInlineAsm,
       getModuleContext,

       -- * Types
       getTypeKind,
       typeIsSized,
       getTypeContext,

       -- * Module providers
       createModuleProviderForExistingModule,
       ptrDisposeModuleProvider,

       -- ** Integer types
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

       -- ** Real types
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
       functionType,
       isFunctionVarArg,
       getReturnType,
       countParamTypes,
       getParamTypes,

       -- ** Struct types
       structTypeInContext,
       structType,
       structCreateNamed,
       getStructName,
       structSetBody,
       countStructElementTypes,
       getStructElementTypes,
       isPackedStruct,
       isOpaqueStruct,
       getTypeByName,

       -- ** Array, pointer, and vector types
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
       typeOf,
       getValueName,
       setValueName,
       dumpValue,
       replaceAllUsesWith,
       hasMetadata,
       getMetadata,
       setMetadata,

       -- ** Uses
       getFirstUse,
       getNextUse,
       getUser,
       getUsedValue,

       -- ** Users
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

       -- ** Metadata
       mdStringInContext,
       mdString,
       mdNodeInContext,
       mdNode,
       getMDString,
       getNamedMetadataNumOperands,
       getNamedMetadataOperands,
       addNamedMetadataOperand,

       -- ** Scalar constants
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
       constStringInContext,
       constStructInContext,
       constString,
       constArray,
       constStruct,
       constNamedStruct,
       constVector,

       -- ** Constant Expressions
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

       -- ** Global variables
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
       addAlias,

       -- ** Functions
       addFunction,
       getNamedFunction,
       getFirstFunction,
       getLastFunction,
       getNextFunction,
       getPreviousFunction,
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

       -- ** Basic blocks
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
       getInstructionParent,
       getNextInstruction,
       getPreviousInstruction,
       instructionEraseFromParent,
       getInstructionOpcode,
       getICmpPredicate,

       -- ** Call Sites
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
       createBuilderInContext,
       createBuilder,
       positionBuilder,
       positionBefore,
       positionAtEnd,
       getInsertBlock,
       clearInsertionPosition,
       insertIntoBuilder,
       insertIntoBuilderWithName,
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

       -- ** PassRegistry
       getGlobalPassRegistry,

       -- ** Pass manager
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
type ContextRef = Ptr Context

data Module
    deriving (Typeable)
type ModuleRef = Ptr Module

data ModuleProvider
    deriving (Typeable)
type ModuleProviderRef = Ptr ModuleProvider

data Type
    deriving (Typeable)
type TypeRef = Ptr Type

type BasicBlock = Value
type BasicBlockRef = Ptr BasicBlock

data Value
    deriving (Typeable)
type ValueRef = Ptr Value

data OpaqueUse
    deriving (Typeable)
type UseRef = Ptr OpaqueUse

data Builder
    deriving (Typeable)
type BuilderRef = Ptr Builder

data MemoryBuffer
    deriving (Typeable)
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

-- ** Error Handling
foreign import ccall unsafe "LLVMDisposeMessage"
  disposeMessage :: CString -> IO ()

-- ** Contexts
foreign import ccall unsafe "LLVMContextCreate"
  contextCreate :: IO ContextRef

foreign import ccall unsafe "LLVMGetGlobalContext"
  getGlobalContext :: IO ContextRef

foreign import ccall unsafe "LLVMContextDispose"
  contextDispose :: ContextRef -> IO ()

foreign import ccall unsafe "LLVMGetMDKindIDInContext"
  getMDKindIDInContext :: ContextRef -> CString -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVMGetMDKindID"
  getMDKindID :: CString -> CUInt -> IO CUInt


-- ** Modules
foreign import ccall unsafe "LLVMModuleCreateWithName"
  moduleCreateWithName :: CString -> IO ModuleRef

foreign import ccall unsafe "LLVMModuleCreateWithNameInContext"
 moduleCreateWithNameInContext :: CString -> ContextRef -> IO ModuleRef

foreign import ccall unsafe "LLVMDisposeModule"
  disposeModule :: ModuleRef -> IO ()

foreign import ccall unsafe "&LLVMDisposeModule"
  ptrDisposeModule :: FunPtr (ModuleRef -> IO ())

-- ** Data Layout
foreign import ccall unsafe "LLVMGetDataLayout"
  getDataLayout :: ModuleRef -> IO CString

foreign import ccall unsafe "LLVMSetDataLayout"
  setDataLayout :: ModuleRef -> CString -> IO ()

-- ** Targets
foreign import ccall unsafe "LLVMGetTarget"
  getTarget :: ModuleRef -> IO CString

foreign import ccall unsafe "LLVMSetTarget"
  setTarget :: ModuleRef -> CString -> IO ()

-- ** Dump module
foreign import ccall unsafe "LLVMDumpModule"
  dumpModule :: ModuleRef -> IO ()

foreign import ccall unsafe "LLVMSetModuleInlineAsm"
  setModuleInlineAsm :: ModuleRef -> CString -> IO ()

foreign import ccall unsafe "LLVMGetModuleContext"
  getModuleContext :: ModuleRef -> IO ContextRef

-- ** Types
foreign import ccall unsafe "LLVMGetTypeKind"
  getTypeKind :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMTypeIsSized"
  typeIsSized :: TypeRef -> IO CInt

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

-- ** Real types
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

foreign import ccall unsafe "LLVMFloatType" floatType :: TypeRef
foreign import ccall unsafe "LLVMDoubleType" doubleType :: TypeRef
foreign import ccall unsafe "LLVMX86FP80Type" x86FP80Type :: TypeRef
foreign import ccall unsafe "LLVMFP128Type" fp128Type :: TypeRef
foreign import ccall unsafe "LLVMPPCFP128Type" ppcFP128Type :: TypeRef

-- ** Function types
-- | Create a function type.
foreign import ccall unsafe "LLVMFunctionType"
  functionType :: TypeRef              -- ^ return type
                  -> Ptr TypeRef          -- ^ array of argument types
                  -> CUInt                -- ^ number of elements in array
                  -> CInt                 -- ^ non-zero if function is varargs
                  -> TypeRef

-- | Indicate whether a function takes varargs.
foreign import ccall unsafe "LLVMIsFunctionVarArg"
  isFunctionVarArg :: TypeRef -> IO CInt

-- | Give a function's return type.
foreign import ccall unsafe "LLVMGetReturnType"
  getReturnType :: TypeRef -> IO TypeRef

-- | Give the number of fixed parameters that a function takes.
foreign import ccall unsafe "LLVMCountParamTypes"
  countParamTypes :: TypeRef -> IO CUInt

-- | Fill out an array with the types of a function's fixed
-- parameters.
foreign import ccall unsafe "LLVMGetParamTypes"
  getParamTypes :: TypeRef -> Ptr TypeRef -> IO ()

-- ** Struct Type
foreign import ccall unsafe "LLVMStructTypeInContext"
  structTypeInContext :: ContextRef -> (Ptr TypeRef) -> CUInt -> CInt ->
                         IO TypeRef

foreign import ccall unsafe "LLVMStructType"
  structType :: Ptr TypeRef -> CUInt -> CInt -> TypeRef

foreign import ccall unsafe "LLVMStructCreateNamed"
  structCreateNamed :: ContextRef -> CString -> IO TypeRef

foreign import ccall unsafe "LLVMGetStructName"
  getStructName :: TypeRef -> CString

foreign import ccall unsafe "LLVMStructSetBody"
  structSetBody :: TypeRef -> Ptr TypeRef -> CUInt -> CInt -> IO ()

foreign import ccall unsafe "LLVMCountStructElementTypes"
    countStructElementTypes :: TypeRef -> CUInt

foreign import ccall unsafe "LLVMGetStructElementTypes"
  getStructElementTypes :: TypeRef -> Ptr TypeRef -> IO ()

foreign import ccall unsafe "LLVMIsPackedStruct"
  isPackedStruct :: TypeRef -> CInt

foreign import ccall unsafe "LLVMIsOpaqueStruct"
  isOpaqueStruct :: TypeRef -> CInt

foreign import ccall unsafe "LLVMGetTypeByName"
  getTypeByName :: ModuleRef -> CString -> IO TypeRef

-- ** Array, Pointer, and Vector types
foreign import ccall unsafe "LLVMArrayType" arrayType
    :: TypeRef                  -- ^ element type
    -> CUInt                    -- ^ element count
    -> TypeRef

foreign import ccall unsafe "LLVMPointerType" pointerType
    :: TypeRef                  -- ^ pointed-to type
    -> CUInt                    -- ^ address space
    -> TypeRef

foreign import ccall unsafe "LLVMVectorType" vectorType
    :: TypeRef                  -- ^ element type
    -> CUInt                    -- ^ element count
    -> TypeRef


-- | Get the type of a sequential type's elements.
foreign import ccall unsafe "LLVMGetElementType"
  getElementType :: TypeRef -> IO TypeRef

foreign import ccall unsafe "LLVMGetArrayLength"
  getArrayLength :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMGetPointerAddressSpace"
  getPointerAddressSpace :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMGetVectorSize"
  getVectorSize :: TypeRef -> IO CUInt

-- ** Other Types

foreign import ccall unsafe "LLVMVoidTypeInContext"
  voidTypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMLabelTypeInContext"
  labelTypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMX86MMXTypeInContext"
  x86MMXTypeInContext :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMVoidType" voidType :: TypeRef
foreign import ccall unsafe "LLVMLabelType" labelType :: TypeRef
foreign import ccall unsafe "LLVMX86MMXType" x86MMXType :: TypeRef

-- ** Values
foreign import ccall unsafe "LLVMTypeOf"
  typeOf :: ValueRef -> IO TypeRef

foreign import ccall unsafe "LLVMGetValueName"
  getValueName :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetValueName"
  setValueName :: ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMDumpValue"
  dumpValue :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMReplaceAllUsesWith"
  replaceAllUsesWith :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMHasMetadata"
  hasMetadata :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMGetMetadata"
  getMetadata :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMSetMetadata"
  setMetadata :: ValueRef -> CUInt -> ValueRef -> IO ()

-- ** Uses
foreign import ccall unsafe "LLVMGetFirstUse"
  getFirstUse :: ValueRef -> IO UseRef

foreign import ccall unsafe "LLVMGetNextUse"
  getNextUse :: UseRef -> IO UseRef

foreign import ccall unsafe "LLVMGetUser"
  getUser :: UseRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetUsedValue"
  getUsedValue :: UseRef -> IO ValueRef

-- ** Users
foreign import ccall unsafe "LLVMGetOperand"
  getOperand :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMSetOperand"
  setOperand :: ValueRef -> CUInt -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetNumOperands"
  getNumOperands :: ValueRef -> IO CUInt

-- ** Constants
foreign import ccall unsafe "LLVMConstNull"
  constNull :: TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstAllOnes"
  constAllOnes :: TypeRef -> ValueRef

foreign import ccall unsafe "LLVMGetUndef"
  getUndef :: TypeRef -> ValueRef

foreign import ccall unsafe "LLVMIsConstant"
  isConstant :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMIsUndef"
  isUndef :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMIsNull"
  isNull :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMConstPointerNull"
  constPointerNull :: TypeRef -> IO ValueRef

-- ** Metadata
foreign import ccall unsafe "LLVMMDStringInContext"
  mdStringInContext :: ContextRef -> CString -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMMDString"
  mdString :: CString -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMMDNodeInContext"
  mdNodeInContext :: ContextRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMMDNode"
  mdNode :: (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMGetMDString"
  getMDString :: ValueRef -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVMGetNamedMetadataNumOperands"
  getNamedMetadataNumOperands :: ModuleRef -> CString -> IO CUInt

foreign import ccall unsafe "LLVMGetNamedMetadataOperands"
  getNamedMetadataOperands :: ModuleRef -> CString -> Ptr ValueRef -> IO ()

foreign import ccall unsafe "LLVMAddNamedMetadataOperand"
  addNamedMetadataOperand :: ModuleRef -> CString -> ValueRef -> IO ()

-- ** Scalar Constants
foreign import ccall unsafe "LLVMConstInt"
  constInt :: TypeRef -> CULLong -> CInt -> ValueRef

foreign import ccall unsafe "LLVMConstIntOfArbitraryPrecision"
  constIntOfArbitraryPrecision :: TypeRef -> CUInt -> Ptr CULLong -> ValueRef

foreign import ccall unsafe "LLVMConstIntOfString"
  constIntOfString :: TypeRef -> CString -> CUChar -> IO ValueRef

foreign import ccall unsafe "LLVMConstIntOfStringAndSize"
  constIntOfStringAndSize :: TypeRef -> CString -> CUInt -> CUChar ->
                             IO ValueRef

foreign import ccall unsafe "LLVMConstReal"
  constReal :: TypeRef -> CDouble -> ValueRef

foreign import ccall unsafe "LLVMConstRealOfString"
  constRealOfString :: TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMConstRealOfStringAndSize"
  constRealOfStringAndSize :: TypeRef -> CString -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstIntGetZExtValue"
  constIntGetZExtValue :: ValueRef -> IO CULLong

foreign import ccall unsafe "LLVMConstIntGetSExtValue"
  constIntGetSExtValue :: ValueRef -> IO CLLong

-- ** Composite Constants
foreign import ccall unsafe "LLVMConstStringInContext"
  constStringInContext :: ContextRef -> CString -> CUInt -> CInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstStructInContext"
  constStructInContext :: ContextRef -> (Ptr ValueRef) -> CUInt -> CInt ->
                          IO ValueRef

foreign import ccall unsafe "LLVMConstString"
  constString :: CString -> CUInt -> CInt -> ValueRef

foreign import ccall unsafe "LLVMConstArray"
  constArray :: TypeRef -> Ptr ValueRef -> CUInt -> ValueRef

foreign import ccall unsafe "LLVMConstStruct"
  constStruct :: Ptr ValueRef -> CUInt -> CInt -> ValueRef

foreign import ccall unsafe "LLVMConstNamedStruct"
  constNamedStruct :: TypeRef -> Ptr ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstVector"
  constVector :: Ptr ValueRef -> CUInt -> ValueRef

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
  constGEP :: ValueRef -> Ptr ValueRef -> CUInt -> ValueRef

foreign import ccall unsafe "LLVMConstInBoundsGEP"
  constInBoundsGEP :: ValueRef -> Ptr ValueRef -> CUInt -> IO ValueRef

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
  constIntCast :: ValueRef -> TypeRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstFPCast"
  constFPCast :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstSelect"
  constSelect :: ValueRef -> ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstExtractElement"
  constExtractElement :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstInsertElement"
  constInsertElement :: ValueRef -> ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstShuffleVector"
  constShuffleVector :: ValueRef -> ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstExtractValue"
  constExtractValue :: ValueRef -> Ptr CUInt -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstInsertValue"
  constInsertValue :: ValueRef -> ValueRef -> Ptr CUInt -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstInlineAsm"
  constInlineAsm :: TypeRef -> CString -> CString -> CInt -> CInt -> IO ValueRef

foreign import ccall unsafe "LLVMBlockAddress"
  blockAddress :: ValueRef -> BasicBlockRef -> IO ValueRef

-- ** Operations on globals
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

-- ** Global Variables
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
  addAlias :: ModuleRef -> TypeRef -> ValueRef -> CString -> IO ValueRef

-- ** Functions
foreign import ccall unsafe "LLVMAddFunction" addFunction
    :: ModuleRef                -- ^ module
    -> CString                  -- ^ name
    -> TypeRef                  -- ^ type
    -> IO ValueRef
foreign import ccall unsafe "LLVMGetNamedFunction" getNamedFunction
    :: ModuleRef                -- ^ module
    -> CString                  -- ^ name
    -> IO ValueRef              -- ^ function (@nullPtr@ if not found)

foreign import ccall unsafe "LLVMGetFirstFunction"
  getFirstFunction :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastFunction"
  getLastFunction :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNextFunction"
  getNextFunction :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousFunction"
  getPreviousFunction :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMDeleteFunction"
  deleteFunction :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetIntrinsicID"
  getIntrinsicID :: ValueRef -> CUInt

foreign import ccall unsafe "LLVMGetFunctionCallConv"
  getFunctionCallConv :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetFunctionCallConv"
  setFunctionCallConv :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetGC"
  getGC :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetGC"
  setGC :: ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMGetFunctionAttr"
  getFunctionAttr :: ValueRef -> IO CUInt {-Attribute-}

foreign import ccall unsafe "LLVMAddFunctionAttr"
  addFunctionAttr :: ValueRef -> CAttribute -> IO ()

foreign import ccall unsafe "LLVMRemoveFunctionAttr"
  removeFunctionAttr :: ValueRef -> CAttribute -> IO ()

-- ** Parameters
foreign import ccall unsafe "LLVMCountParams"
  countParams :: ValueRef -> CUInt

foreign import ccall unsafe "LLVMGetParams"
  getParams :: ValueRef -- ^ function
            -> Ptr ValueRef -- ^ array to fill out
            -> IO ()

foreign import ccall unsafe "LLVMGetParam"
  getParam :: ValueRef -- ^ function
           -> CUInt -- ^ offset into array
           -> ValueRef

foreign import ccall unsafe "LLVMGetParamParent"
  getParamParent :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetFirstParam"
  getFirstParam :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastParam"
  getLastParam :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNextParam"
  getNextParam :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousParam"
  getPreviousParam :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMAddAttribute"
  addAttribute :: ValueRef -> CAttribute -> IO ()

foreign import ccall unsafe "LLVMRemoveAttribute"
  removeAttribute :: ValueRef -> CAttribute -> IO ()

foreign import ccall unsafe "LLVMGetAttribute"
  getAttribute :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetParamAlignment"
  setParamAlignment :: ValueRef -> CUInt -> IO ()

-- ** Basic Blocks
foreign import ccall unsafe "LLVMBasicBlockAsValue"
  basicBlockAsValue :: BasicBlockRef -> ValueRef

foreign import ccall unsafe "LLVMValueIsBasicBlock"
  valueIsBasicBlock :: ValueRef -> CInt

foreign import ccall unsafe "LLVMValueAsBasicBlock"
  valueAsBasicBlock :: ValueRef -- ^ basic block
                    -> BasicBlockRef

foreign import ccall unsafe "LLVMGetBasicBlockParent"
  getBasicBlockParent :: BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetBasicBlockTerminator"
  getBasicBlockTerminator :: BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMCountBasicBlocks"
  countBasicBlocks :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetBasicBlocks"
  getBasicBlocks :: ValueRef -> Ptr BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMGetFirstBasicBlock"
  getFirstBasicBlock :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetLastBasicBlock"
  getLastBasicBlock :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetNextBasicBlock"
  getNextBasicBlock :: BasicBlockRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetPreviousBasicBlock"
  getPreviousBasicBlock :: BasicBlockRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetEntryBasicBlock"
  getEntryBasicBlock :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMAppendBasicBlockInContext"
  appendBasicBlockInContext :: ContextRef -> ValueRef -> CString ->
                               IO BasicBlockRef

foreign import ccall unsafe "LLVMInsertBasicBlockInContext"
  insertBasicBlockInContext :: ContextRef -> BasicBlockRef -> CString ->
                               IO BasicBlockRef

foreign import ccall unsafe "LLVMAppendBasicBlock"
  appendBasicBlock :: ValueRef -> CString -> IO BasicBlockRef

foreign import ccall unsafe "LLVMInsertBasicBlock"
  insertBasicBlock :: BasicBlockRef -> CString -> IO BasicBlockRef

foreign import ccall unsafe "LLVMDeleteBasicBlock"
  deleteBasicBlock :: BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMRemoveBasicBlockFromParent"
  removeBasicBlockFromParent :: BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMMoveBasicBlockBefore"
  moveBasicBlockBefore :: BasicBlockRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMMoveBasicBlockAfter"
  moveBasicBlockAfter :: BasicBlockRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMGetFirstInstruction"
  getFirstInstruction :: BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastInstruction"
  getLastInstruction :: BasicBlockRef -> IO ValueRef

-- ** Instructions
foreign import ccall unsafe "LLVMGetInstructionParent"
  getInstructionParent :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetNextInstruction"
  getNextInstruction :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousInstruction"
  getPreviousInstruction :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMInstructionEraseFromParent"
  instructionEraseFromParent :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetInstructionOpcode"
  getInstructionOpcode :: ValueRef -> IO COpcode

foreign import ccall unsafe "LLVMGetICmpPredicate"
  getICmpPredicate :: ValueRef -> IO CIntPredicate

-- ** Call sites
foreign import ccall unsafe "LLVMSetInstructionCallConv"
  setInstructionCallConv :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetInstructionCallConv"
  getInstructionCallConv :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMAddInstrAttribute"
  addInstrAttribute :: ValueRef -> CUInt -> CAttribute -> IO ()

foreign import ccall unsafe "LLVMRemoveInstrAttribute"
  removeInstrAttribute :: ValueRef -> CUInt -> CAttribute -> IO ()

foreign import ccall unsafe "LLVMSetInstrParamAlignment"
  setInstrParamAlignment :: ValueRef -> CUInt -> CUInt -> IO ()

-- ** Call instructions
foreign import ccall unsafe "LLVMIsTailCall"
  isTailCall :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMSetTailCall"
  setTailCall :: ValueRef -> CInt -> IO ()

-- ** Switch Instructions
foreign import ccall unsafe "LLVMGetSwitchDefaultDest"
  getSwitchDefaultDest :: ValueRef -> IO BasicBlockRef

-- ** Phi Nodes
foreign import ccall unsafe "LLVMAddIncoming"
  addIncoming :: ValueRef -> Ptr ValueRef -> Ptr ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMCountIncoming"
  countIncoming :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetIncomingValue"
  getIncomingValue :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMGetIncomingBlock"
  getIncomingBlock :: ValueRef -> CUInt -> IO BasicBlockRef

-- ** Builders
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

foreign import ccall unsafe "&LLVMDisposeBuilder"
  ptrDisposeBuilder :: FunPtr (BuilderRef -> IO ())

-- ** Metadata
foreign import ccall unsafe "LLVMGetCurrentDebugLocation"
  getCurrentDebugLocation :: BuilderRef -> IO ValueRef

foreign import ccall unsafe "LLVMSetCurrentDebugLocation"
  setCurrentDebugLocation :: BuilderRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMSetInstDebugLocation"
  setInstDebugLocation :: BuilderRef -> ValueRef -> IO ()

-- ** Terminators
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
  buildStore :: BuilderRef -> ValueRef -> ValueRef -> IO ValueRef

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

foreign import ccall unsafe "LLVMBuildFPExt" buildFPExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

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
  buildCall :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString ->
               IO ValueRef

foreign import ccall unsafe "LLVMBuildSelect"
  buildSelect :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString ->
                 IO ValueRef

foreign import ccall unsafe "LLVMBuildVAArg"
  buildVAArg :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildExtractElement"
  buildExtractElement :: BuilderRef -> ValueRef -> ValueRef -> CString ->
                         IO ValueRef

foreign import ccall unsafe "LLVMBuildInsertElement"
  buildInsertElement :: BuilderRef -> ValueRef -> ValueRef -> ValueRef ->
                        CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildShuffleVector"
  buildShuffleVector :: BuilderRef -> ValueRef -> ValueRef -> ValueRef ->
                        CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildExtractValue"
  buildExtractValue :: BuilderRef -> ValueRef -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildInsertValue"
  buildInsertValue :: BuilderRef -> ValueRef -> ValueRef -> CUInt -> CString ->
                      IO ValueRef

foreign import ccall unsafe "LLVMBuildIsNull"
  buildIsNull :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildIsNotNull"
  buildIsNotNull :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildPtrDiff"
  buildPtrDiff :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

-- ** Module Providers
foreign import ccall unsafe "LLVMCreateModuleProviderForExistingModule"
  createModuleProviderForExistingModule :: ModuleRef -> IO ModuleProviderRef

foreign import ccall unsafe "&LLVMDisposeModuleProvider"
  ptrDisposeModuleProvider :: FunPtr (ModuleProviderRef -> IO ())

-- ** Memory Buffers
foreign import ccall unsafe "LLVMCreateMemoryBufferWithContentsOfFile"
  createMemoryBufferWithContentsOfFile :: CString -> Ptr MemoryBufferRef ->
                                          Ptr CString -> IO CInt

foreign import ccall unsafe "LLVMCreateMemoryBufferWithSTDIN"
  createMemoryBufferWithSTDIN :: Ptr MemoryBufferRef -> Ptr CString -> IO CInt

foreign import ccall unsafe "LLVMDisposeMemoryBuffer"
  disposeMemoryBuffer :: MemoryBufferRef -> IO ()

-- ** Pass Registry
foreign import ccall unsafe "LLVMGetGlobalPassRegistry"
  getGlobalPassRegistry :: IO PassRegistryRef

-- ** Pass Managers
foreign import ccall unsafe "LLVMCreatePassManager"
  createPassManager :: IO PassManagerRef

foreign import ccall unsafe "LLVMCreateFunctionPassManagerForModule"
  createFunctionPassManagerForModule :: ModuleRef -> IO PassManagerRef

foreign import ccall unsafe "LLVMCreateFunctionPassManager"
  createFunctionPassManager :: ModuleProviderRef -> IO PassManagerRef

foreign import ccall unsafe "LLVMRunPassManager"
  runPassManager :: PassManagerRef -> ModuleRef -> IO CInt

foreign import ccall unsafe "LLVMInitializeFunctionPassManager"
  initializeFunctionPassManager :: PassManagerRef -> IO CInt

foreign import ccall unsafe "LLVMRunFunctionPassManager"
  runFunctionPassManager :: PassManagerRef -> ValueRef -> IO CInt

foreign import ccall unsafe "LLVMFinalizeFunctionPassManager"
  finalizeFunctionPassManager :: PassManagerRef -> IO CInt

foreign import ccall unsafe "LLVMDisposePassManager"
  disposePassManager :: PassManagerRef -> IO ()

foreign import ccall unsafe "&LLVMDisposePassManager"
  ptrDisposePassManager :: FunPtr (PassManagerRef -> IO ())
