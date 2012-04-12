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
--       getFunctionAttr,
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
--       getAttribute,
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
       createPassManager,
       createFunctionPassManagerForModule,
       createFunctionPassManager,
       runPassManager,
       initializeFunctionPassManager,
       runFunctionPassManager,
       finalizeFunctionPassManager,
       disposePassManager
       ) where

import Foreign hiding (sizeOf)
import Foreign.C.String
import Foreign.C.Types
import LLVM.FFI.Core(ContextRef, ModuleRef, TypeRef, ValueRef,
                     BasicBlockRef, BuilderRef, MemoryBufferRef,
                     PassManagerRef, PassRegistryRef, Attribute(..),
                     TypeKind(..), Linkage(..), Visibility(..),
                     IntPredicate(..), RealPredicate(..),
                     CallingConvention(..), Opcode(..))

import qualified LLVM.FFI.Core as FFI

getMDKindIDInContext :: Num n => ContextRef -> String -> IO n
getMDKindIDInContext ctx str =
  withCString str
    (\cstr ->
      FFI.getMDKindIDInContext ctx cstr (fromIntegral (length str)) >>=
        return . fromIntegral)

getMDKindID :: Num n => String -> IO n
getMDKindID str =
  withCString str
    (\cstr ->
      FFI.getMDKindID cstr (fromIntegral (length str)) >>=
        return . fromIntegral)

moduleCreateWithName :: String -> IO ModuleRef
moduleCreateWithName str = withCString str FFI.moduleCreateWithName

moduleCreateWithNameInContext :: String -> ContextRef -> IO ModuleRef
moduleCreateWithNameInContext str ctx =
  withCString str (\cstr -> FFI.moduleCreateWithNameInContext cstr ctx)

getDataLayout :: ModuleRef -> IO String
getDataLayout mod = FFI.getDataLayout mod >>= peekCString

setDataLayout :: ModuleRef -> String -> IO ()
setDataLayout mod str = withCString str (FFI.setDataLayout mod)

getTarget :: ModuleRef -> IO String
getTarget mod = FFI.getTarget mod >>= peekCString

setTarget :: ModuleRef -> String -> IO ()
setTarget mod str = withCString str (FFI.setTarget mod)

setModuleInlineAsm :: ModuleRef -> String -> IO ()
setModuleInlineAsm mod str = withCString str (FFI.setModuleInlineAsm mod)

getTypeKind :: TypeRef -> IO TypeKind
getTypeKind ty = FFI.getTypeKind ty >>= return . FFI.toTypeKind

typeIsSized :: TypeRef -> IO Bool
typeIsSized ty = FFI.typeIsSized ty >>= return . toBool

intTypeInContext :: Integral n => ContextRef -> n -> IO TypeRef
intTypeInContext ctx = FFI.intTypeInContext ctx . fromIntegral

intType :: Integral n => n -> TypeRef
intType = FFI.intType . fromIntegral

getIntTypeWidth :: Num n => TypeRef -> IO n
getIntTypeWidth tyref = FFI.getIntTypeWidth tyref >>= return . fromIntegral

functionType :: TypeRef -> [TypeRef] -> Bool -> TypeRef
functionType retty params vararg =
  unsafePerformIO
    (withArrayLen params
      (\len arr ->
        return (FFI.functionType retty arr (fromIntegral len)
                                 (fromBool vararg))))

isFunctionVarArg :: TypeRef -> IO Bool
isFunctionVarArg func = FFI.isFunctionVarArg func >>= return . toBool

countParamTypes :: Num n => TypeRef -> IO n
countParamTypes func = FFI.countParamTypes func >>= return . fromIntegral

getParamTypes :: TypeRef -> IO [TypeRef]
getParamTypes func =
  do
    len <- countParamTypes func
    allocaArray len (\arr -> FFI.getParamTypes func arr >> peekArray len arr)

structTypeInContext :: ContextRef -> [TypeRef] -> Bool -> IO TypeRef
structTypeInContext ctx fields packed =
  withArrayLen fields
    (\len arr -> FFI.structTypeInContext ctx arr (fromIntegral len)
                                         (fromBool packed))

structType :: [TypeRef] -> Bool -> TypeRef
structType fields packed =
  unsafePerformIO
    (withArrayLen fields
      (\len arr ->
        return (FFI.structType arr (fromIntegral len) (fromBool packed))))

structCreateNamed :: ContextRef -> String -> IO TypeRef
structCreateNamed ctx str = withCString str (FFI.structCreateNamed ctx)

getStructName :: TypeRef -> String
getStructName ty = unsafePerformIO (peekCString (FFI.getStructName ty))

structSetBody :: TypeRef -> [TypeRef] -> Bool -> IO ()
structSetBody ty fields packed =
  withArrayLen fields
    (\len arr -> FFI.structSetBody ty arr (fromIntegral len) (fromBool packed))

countStructElementTypes :: Num n => TypeRef -> n
countStructElementTypes = fromIntegral . FFI.countStructElementTypes

getStructElementTypes :: TypeRef -> IO [TypeRef]
getStructElementTypes struct =
  let
    len = countStructElementTypes struct
  in
    allocaArray len
      (\arr -> FFI.getStructElementTypes struct arr >> peekArray len arr)

isPackedStruct :: TypeRef -> Bool
isPackedStruct = toBool . FFI.isPackedStruct

isOpaqueStruct :: TypeRef -> Bool
isOpaqueStruct = toBool . FFI.isOpaqueStruct

getTypeByName :: ModuleRef -> String -> IO TypeRef
getTypeByName mod str = withCString str (FFI.getTypeByName mod)

arrayType :: Integral n => TypeRef -> n -> TypeRef
arrayType ty = FFI.arrayType ty . fromIntegral

pointerType :: Integral n => TypeRef -> n -> TypeRef
pointerType ty = FFI.pointerType ty . fromIntegral

vectorType :: Integral n => TypeRef -> n -> TypeRef
vectorType ty = FFI.vectorType ty . fromIntegral

getArrayLength :: Num n => TypeRef -> IO n
getArrayLength arr = FFI.getArrayLength arr >>= return . fromIntegral

getPointerAddressSpace :: Num n => TypeRef -> IO n
getPointerAddressSpace arr =
  FFI.getPointerAddressSpace arr >>= return . fromIntegral

getVectorSize :: Num n => TypeRef -> IO n
getVectorSize arr = FFI.getVectorSize arr >>= return . fromIntegral

getValueName :: ValueRef -> IO String
getValueName val = FFI.getValueName val >>= peekCString

setValueName :: ValueRef -> String -> IO ()
setValueName val str = withCString str (FFI.setValueName val)

hasMetadata :: ValueRef -> IO Bool
hasMetadata val = FFI.hasMetadata val >>= return . toBool

getMetadata :: Integral n => ValueRef -> n -> IO ValueRef
getMetadata val = FFI.getMetadata val . fromIntegral

setMetadata :: Integral n => ValueRef -> n -> ValueRef -> IO ()
setMetadata val ind mdata = FFI.setMetadata val (fromIntegral ind) mdata

getOperand :: Integral n => ValueRef -> n -> IO ValueRef
getOperand val = FFI.getOperand val . fromIntegral

setOperand :: Integral n => ValueRef -> n -> ValueRef -> IO ()
setOperand val ind mdata = FFI.setOperand val (fromIntegral ind) mdata

getNumOperands :: Integral n => ValueRef -> IO n
getNumOperands val = FFI.getNumOperands val >>= return . fromIntegral

isConstant :: ValueRef -> IO Bool
isConstant val = FFI.isConstant val >>= return . toBool

isUndef :: ValueRef -> IO Bool
isUndef val = FFI.isUndef val >>= return . toBool

isNull :: ValueRef -> IO Bool
isNull val = FFI.isNull val >>= return . toBool

mdStringInContext :: ContextRef -> String -> IO ValueRef
mdStringInContext ctx str =
  withCStringLen str
    (\(cstr, len) -> FFI.mdStringInContext ctx cstr (fromIntegral len))

mdString :: String -> IO ValueRef
mdString str =
  withCStringLen str (\(cstr, len) -> FFI.mdString cstr (fromIntegral len))

mdNodeInContext :: ContextRef -> [ValueRef] -> IO ValueRef
mdNodeInContext ctx vals =
  withArrayLen vals (\len arr -> FFI.mdNodeInContext ctx arr (fromIntegral len))

mdNode :: [ValueRef] -> IO ValueRef
mdNode vals =
  withArrayLen vals (\len arr -> FFI.mdNode arr (fromIntegral len))

readMDString :: ValueRef -> Ptr CUInt -> IO String
readMDString val ptr =
  do
    cstr <- FFI.getMDString val ptr
    len <- peek ptr
    peekCStringLen (cstr, fromIntegral len)

getMDString :: ValueRef -> IO String
getMDString val = alloca (readMDString val)

getNamedMetadataNumOperands :: Integral n => ModuleRef -> String -> IO n
getNamedMetadataNumOperands mod str =
  withCString str
    (\cstr -> FFI.getNamedMetadataNumOperands mod cstr >>=
                return . fromIntegral)

getNamedMetadataOperands :: ModuleRef -> String -> IO [ValueRef]
getNamedMetadataOperands mod str =
  do
    len <- getNamedMetadataNumOperands mod str
    allocaArray (fromIntegral len)
      (\arr -> withCString str
        (\cstr -> FFI.getNamedMetadataOperands mod cstr arr >>
                    peekArray len arr))

addNamedMetadataOperand :: ModuleRef -> String -> ValueRef -> IO ()
addNamedMetadataOperand mod str val =
  withCString str (\cstr -> FFI.addNamedMetadataOperand mod cstr val)

constInt :: Integral n => TypeRef -> n -> Bool -> ValueRef
constInt ty val sext = FFI.constInt ty (fromIntegral val) (fromBool sext)

constIntOfArbitraryPrecision :: TypeRef -> [Word64] -> ValueRef
constIntOfArbitraryPrecision ty vals =
  unsafePerformIO
    (withArrayLen (map fromIntegral vals)
      (\len arr ->
        return (FFI.constIntOfArbitraryPrecision ty (fromIntegral len) arr)))

constIntOfString :: Integral n => TypeRef -> String -> n -> IO ValueRef
constIntOfString ty str radix =
  withCString str (\cstr -> FFI.constIntOfString ty cstr (fromIntegral radix))

constIntOfStringAndSize :: (Integral m, Integral n) =>
                           TypeRef -> String -> m -> n -> IO ValueRef
constIntOfStringAndSize ty str size radix =
  withCString str
    (\cstr -> FFI.constIntOfStringAndSize ty cstr (fromIntegral size)
                                          (fromIntegral radix))

constReal :: Real n => TypeRef -> n -> ValueRef
constReal ty val = FFI.constReal ty (realToFrac val)

constRealOfString :: TypeRef -> String -> IO ValueRef
constRealOfString ty str = withCString str (FFI.constRealOfString ty)

constRealOfStringAndSize :: Integral n => TypeRef -> String -> n -> IO ValueRef
constRealOfStringAndSize ty str size =
  withCString str
    (\cstr -> FFI.constRealOfStringAndSize ty cstr (fromIntegral size))

constIntGetZExtValue :: Integral n => ValueRef -> IO n
constIntGetZExtValue val =
  FFI.constIntGetZExtValue val >>= return . fromIntegral

constIntGetSExtValue :: Integral n => ValueRef -> IO n
constIntGetSExtValue val =
  FFI.constIntGetSExtValue val >>= return . fromIntegral

constStringInContext :: ContextRef -> String -> Bool -> IO ValueRef
constStringInContext ctx str nullterm =
  withCStringLen str
    (\(cstr, len) ->
      FFI.constStringInContext ctx cstr (fromIntegral len) (fromBool nullterm))

constStructInContext :: ContextRef -> [ValueRef] -> Bool -> IO ValueRef
constStructInContext ctx vals packed =
  withArrayLen vals
    (\len arr ->
      FFI.constStructInContext ctx arr (fromIntegral len) (fromBool packed))

constString :: String -> Bool -> ValueRef
constString str nullterm =
  unsafePerformIO
    (withCStringLen str
      (\(cstr, len) ->
        return (FFI.constString cstr (fromIntegral len) (fromBool nullterm))))

constArray :: TypeRef -> [ValueRef] -> ValueRef
constArray ty vals =
  unsafePerformIO
    (withArrayLen vals
      (\len arr -> return (FFI.constArray ty arr (fromIntegral len))))

constStruct :: [ValueRef] -> Bool -> ValueRef
constStruct vals packed =
  unsafePerformIO
    (withArrayLen vals
      (\len arr ->
        return (FFI.constStruct arr (fromIntegral len) (fromBool packed))))

constNamedStruct :: TypeRef -> [ValueRef] -> IO ValueRef
constNamedStruct ty vals =
  withArrayLen vals
    (\len arr -> FFI.constNamedStruct ty arr (fromIntegral len))

constVector :: [ValueRef] -> ValueRef
constVector vals =
  unsafePerformIO
    (withArrayLen vals
      (\len arr -> return (FFI.constVector arr (fromIntegral len))))

getConstOpcode :: ValueRef -> IO Opcode
getConstOpcode val = FFI.getConstOpcode val >>= return . FFI.toOpcode

constICmp :: IntPredicate -> ValueRef -> ValueRef -> ValueRef
constICmp pred = FFI.constICmp (FFI.fromIntPredicate pred)

constFCmp :: RealPredicate -> ValueRef -> ValueRef -> ValueRef
constFCmp pred = FFI.constFCmp (FFI.fromRealPredicate pred)

constGEP :: ValueRef -> [ValueRef] -> ValueRef
constGEP val offs =
  unsafePerformIO
    (withArrayLen offs
      (\len arr -> return (FFI.constGEP val arr (fromIntegral len))))

constInBoundsGEP :: ValueRef -> [ValueRef] -> IO ValueRef
constInBoundsGEP val offs =
  withArrayLen offs
    (\len arr -> FFI.constInBoundsGEP val arr (fromIntegral len))

constIntCast :: ValueRef -> TypeRef -> Bool -> IO ValueRef
constIntCast val ty = FFI.constIntCast val ty . fromBool

constExtractValue :: Integral n => ValueRef -> [n] -> IO ValueRef
constExtractValue val offs =
  withArrayLen (map fromIntegral offs)
    (\len arr -> FFI.constExtractValue val arr (fromIntegral len))

constInsertValue :: Integral n => ValueRef -> ValueRef -> [n] -> IO ValueRef
constInsertValue val elem offs =
  withArrayLen (map fromIntegral offs)
    (\len arr -> FFI.constInsertValue val elem arr (fromIntegral len))

constInlineAsm :: TypeRef -> String -> String -> Bool -> Bool -> IO ValueRef
constInlineAsm ty asm const effects align =
  withCString asm
    (\casm -> withCString const
      (\cconst ->
        FFI.constInlineAsm ty casm cconst (fromBool effects) (fromBool align)))

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

addGlobal :: ModuleRef -> TypeRef -> String -> IO ValueRef
addGlobal mod ty str =
  withCString str (FFI.addGlobal mod ty)

addGlobalInAddressSpace :: Integral n =>
                           ModuleRef -> TypeRef -> String -> n -> IO ValueRef
addGlobalInAddressSpace mod ty str spc =
  withCString str
    (\cstr -> FFI.addGlobalInAddressSpace mod ty cstr (fromIntegral spc))

getNamedGlobal :: ModuleRef -> String -> IO ValueRef
getNamedGlobal mod str =
  withCString str (FFI.getNamedGlobal mod)

isThreadLocal :: ValueRef -> IO Bool
isThreadLocal val = FFI.isThreadLocal val >>= return . toBool

setThreadLocal :: ValueRef -> Bool -> IO ()
setThreadLocal val = FFI.setThreadLocal val . fromBool

isGlobalConstant :: ValueRef -> IO Bool
isGlobalConstant val = FFI.isGlobalConstant val >>= return . toBool

setGlobalConstant :: ValueRef -> Bool -> IO ()
setGlobalConstant val = FFI.setGlobalConstant val . fromBool

addAlias :: ModuleRef -> TypeRef -> ValueRef -> String -> IO ValueRef
addAlias mod ty val str = withCString str (FFI.addAlias mod ty val)

addFunction :: ModuleRef -> String -> TypeRef -> IO ValueRef
addFunction mod str ty = withCString str (\cstr -> FFI.addFunction mod cstr ty)

getNamedFunction :: ModuleRef -> String -> IO ValueRef
getNamedFunction mod str = withCString str (FFI.getNamedFunction mod)

getIntrinsicID :: Integral n => ValueRef -> n
getIntrinsicID = fromIntegral . FFI.getIntrinsicID

getFunctionCallConv :: ValueRef -> IO CallingConvention
getFunctionCallConv val =
  FFI.getFunctionCallConv val >>= return . FFI.toCallingConvention

setFunctionCallConv :: ValueRef -> CallingConvention -> IO ()
setFunctionCallConv val =
  FFI.setFunctionCallConv val . FFI.fromCallingConvention

getGC :: ValueRef -> IO String
getGC val = FFI.getGC val >>= peekCString

setGC :: ValueRef -> String -> IO ()
setGC val str = withCString str (FFI.setGC val)

addFunctionAttr :: ValueRef -> Attribute -> IO ()
addFunctionAttr val = FFI.addFunctionAttr val . FFI.fromAttribute

removeFunctionAttr :: ValueRef -> Attribute -> IO ()
removeFunctionAttr val = FFI.removeFunctionAttr val . FFI.fromAttribute

countParams :: Integral n => ValueRef -> n
countParams = fromIntegral . FFI.countParams

getParams :: ValueRef -> IO [ValueRef]
getParams func =
  let
    len = countParams func
  in
    allocaArray len (\arr -> FFI.getParams func arr >> peekArray len arr)

getParam :: Integral n => ValueRef -> n -> ValueRef
getParam val = FFI.getParam val . fromIntegral

addAttribute :: ValueRef -> Attribute -> IO ()
addAttribute val = FFI.addAttribute val . FFI.fromAttribute

removeAttribute :: ValueRef -> Attribute -> IO ()
removeAttribute val = FFI.removeAttribute val . FFI.fromAttribute

setParamAlignment :: Integral n => ValueRef -> n -> IO ()
setParamAlignment val = FFI.setParamAlignment val . fromIntegral

valueIsBasicBlock :: ValueRef -> Bool
valueIsBasicBlock = toBool . FFI.valueIsBasicBlock

countBasicBlocks :: Integral n => ValueRef -> IO n
countBasicBlocks val = FFI.countBasicBlocks val >>= return . fromIntegral

getBasicBlocks :: ValueRef -> IO [BasicBlockRef]
getBasicBlocks func =
  do
    num <- countBasicBlocks func
    allocaArray num (\arr -> FFI.getBasicBlocks func arr >> peekArray num arr)

appendBasicBlockInContext :: ContextRef -> ValueRef -> String ->
                             IO BasicBlockRef
appendBasicBlockInContext ctx func str =
  withCString str (FFI.appendBasicBlockInContext ctx func)

insertBasicBlockInContext :: ContextRef -> BasicBlockRef -> String ->
                             IO BasicBlockRef
insertBasicBlockInContext ctx func str =
  withCString str (FFI.insertBasicBlockInContext ctx func)

appendBasicBlock :: ValueRef -> String -> IO BasicBlockRef
appendBasicBlock func str = withCString str (FFI.appendBasicBlock func)

insertBasicBlock :: BasicBlockRef -> String -> IO BasicBlockRef
insertBasicBlock func str = withCString str (FFI.insertBasicBlock func)

getInstructionOpcode :: ValueRef -> IO Opcode
getInstructionOpcode val =
  FFI.getInstructionOpcode val >>= return . FFI.toOpcode

getICmpPredicate :: ValueRef -> IO IntPredicate
getICmpPredicate val = FFI.getICmpPredicate val >>= return . FFI.toIntPredicate

setInstructionCallConv :: ValueRef -> CallingConvention -> IO ()
setInstructionCallConv val =
  FFI.setInstructionCallConv val . FFI.fromCallingConvention 

getInstructionCallConv :: ValueRef -> IO CallingConvention
getInstructionCallConv val =
  FFI.getInstructionCallConv val >>= return . FFI.toCallingConvention

addInstrAttribute :: Integral n => ValueRef -> n -> Attribute -> IO ()
addInstrAttribute val n =
  FFI.addInstrAttribute val (fromIntegral n) . FFI.fromAttribute

removeInstrAttribute :: Integral n => ValueRef -> n -> Attribute -> IO ()
removeInstrAttribute val n =
  FFI.removeInstrAttribute val (fromIntegral n) . FFI.fromAttribute

setInstrParamAlignment :: (Integral n, Integral m) =>
                          ValueRef -> m -> n -> IO ()
setInstrParamAlignment val ind =
  FFI.setInstrParamAlignment val (fromIntegral ind) . fromIntegral

isTailCall :: ValueRef -> IO Bool
isTailCall val = FFI.isTailCall val >>= return . toBool

setTailCall :: ValueRef -> Bool -> IO ()
setTailCall val = FFI.setTailCall val . fromBool

addIncoming :: ValueRef -> [(ValueRef, ValueRef)] -> IO ()
addIncoming val ins =
  let
    (vals, blocks) = unzip ins
  in
    withArray vals
      (\varr -> withArrayLen blocks
        (\len barr -> FFI.addIncoming val barr varr (fromIntegral len)))

countIncoming :: Integral n => ValueRef -> IO n
countIncoming val = FFI.countIncoming val >>= return . fromIntegral

getIncomingValue :: Integral n => ValueRef -> n -> IO ValueRef
getIncomingValue val = FFI.getIncomingValue val . fromIntegral

getIncomingBlock :: Integral n => ValueRef -> n -> IO BasicBlockRef
getIncomingBlock val = FFI.getIncomingBlock val . fromIntegral

insertIntoBuilderWithName :: BuilderRef -> ValueRef -> String -> IO ()
insertIntoBuilderWithName b val str =
  withCString str (FFI.insertIntoBuilderWithName b val)

buildAggregateRet :: BuilderRef -> [ValueRef] -> IO ValueRef
buildAggregateRet b vals =
  withArrayLen vals (\len arr -> FFI.buildAggregateRet b arr (fromIntegral len))

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

setCleanup :: ValueRef -> Bool -> IO ()
setCleanup val = FFI.setCleanup val . fromBool

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

buildMalloc :: BuilderRef -> TypeRef -> String -> IO ValueRef
buildMalloc b ty str = withCString str (FFI.buildMalloc b ty)

buildArrayMalloc :: BuilderRef -> TypeRef -> ValueRef -> String -> IO ValueRef
buildArrayMalloc b ty val str = withCString str (FFI.buildArrayMalloc b ty val)

buildAlloca :: BuilderRef -> TypeRef -> String -> IO ValueRef
buildAlloca b ty str = withCString str (FFI.buildAlloca b ty)

buildArrayAlloca :: BuilderRef -> TypeRef -> ValueRef -> String -> IO ValueRef
buildArrayAlloca b ty val str = withCString str (FFI.buildArrayAlloca b ty val)

buildLoad :: BuilderRef -> ValueRef -> String -> IO ValueRef
buildLoad b val str = withCString str (FFI.buildLoad b val)

buildGEP :: BuilderRef -> ValueRef -> [ValueRef] -> String -> IO ValueRef
buildGEP b val inds str =
  withArrayLen inds
    (\len arr -> withCString str
      (\cstr -> FFI.buildGEP b val arr (fromIntegral len) cstr))

buildInBoundsGEP :: BuilderRef -> ValueRef -> [ValueRef] -> String ->
                    IO ValueRef
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

buildICmp :: BuilderRef -> IntPredicate -> ValueRef -> ValueRef -> String ->
             IO ValueRef
buildICmp b pred l r str =
  withCString str (FFI.buildICmp b (FFI.fromIntPredicate pred) l r)

buildFCmp :: BuilderRef -> RealPredicate -> ValueRef -> ValueRef -> String ->
             IO ValueRef
buildFCmp b pred l r str =
  withCString str (FFI.buildFCmp b (FFI.fromRealPredicate pred) l r)

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

runPassManager :: PassManagerRef -> ModuleRef -> IO Bool
runPassManager mgr mod = FFI.runPassManager mgr mod >>= return . toBool

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

initializeFunctionPassManager :: PassManagerRef -> IO Bool
initializeFunctionPassManager mgr =
  FFI.initializeFunctionPassManager mgr >>= return . toBool

runFunctionPassManager :: PassManagerRef -> ValueRef -> IO Bool
runFunctionPassManager mgr val =
  FFI.runFunctionPassManager mgr val >>= return . toBool

finalizeFunctionPassManager :: PassManagerRef -> IO Bool
finalizeFunctionPassManager mgr =
  FFI.finalizeFunctionPassManager mgr >>= return . toBool

disposeMessage = FFI.disposeMessage
contextCreate = FFI.contextCreate
getGlobalContext = FFI.getGlobalContext
contextDispose = FFI.contextDispose
disposeModule = FFI.disposeModule
dumpModule = FFI.dumpModule
getModuleContext = FFI.getModuleContext
getTypeContext = FFI.getTypeContext
int1TypeInContext = FFI.int1TypeInContext
int8TypeInContext = FFI.int8TypeInContext
int16TypeInContext = FFI.int16TypeInContext
int32TypeInContext = FFI.int32TypeInContext
int64TypeInContext = FFI.int64TypeInContext
int1Type = FFI.int1Type
int8Type = FFI.int8Type
int16Type = FFI.int16Type
int32Type = FFI.int32Type
int64Type = FFI.int64Type
floatTypeInContext = FFI.floatTypeInContext
doubleTypeInContext = FFI.doubleTypeInContext
x86FP80TypeInContext = FFI.x86FP80TypeInContext
fp128TypeInContext = FFI.fp128TypeInContext
ppcFP128TypeInContext = FFI.ppcFP128TypeInContext
floatType = FFI.floatType
doubleType = FFI.doubleType
x86FP80Type = FFI.x86FP80Type
fp128Type = FFI.fp128Type
ppcFP128Type = FFI.ppcFP128Type
getReturnType = FFI.getReturnType
getElementType = FFI.getElementType
voidTypeInContext = FFI.voidTypeInContext
labelTypeInContext = FFI.labelTypeInContext
x86MMXTypeInContext = FFI.x86MMXTypeInContext
voidType = FFI.voidType
labelType = FFI.labelType
x86MMXType = FFI.x86MMXType
typeOf = FFI.typeOf
dumpValue = FFI.dumpValue
replaceAllUsesWith = FFI.replaceAllUsesWith
getFirstUse = FFI.getFirstUse
getNextUse = FFI.getNextUse
getUser = FFI.getUser
getUsedValue = FFI.getUsedValue
constNull = FFI.constNull
constAllOnes = FFI.constAllOnes
getUndef = FFI.getUndef
constPointerNull = FFI.constPointerNull
alignOf = FFI.alignOf
sizeOf = FFI.sizeOf
constNeg = FFI.constNeg
constNSWNeg = FFI.constNSWNeg
constNUWNeg = FFI.constNUWNeg
constFNeg = FFI.constFNeg
constNot = FFI.constNot
constAdd = FFI.constAdd
constNSWAdd = FFI.constNSWAdd
constNUWAdd = FFI.constNUWAdd
constFAdd = FFI.constFAdd
constSub = FFI.constSub
constNSWSub = FFI.constNSWSub
constNUWSub = FFI.constNUWSub
constFSub = FFI.constFSub
constMul = FFI.constMul
constNSWMul = FFI.constNSWMul
constNUWMul = FFI.constNUWMul
constFMul = FFI.constFMul
constUDiv = FFI.constUDiv
constSDiv = FFI.constSDiv
constExactSDiv = FFI.constExactSDiv
constFDiv = FFI.constFDiv
constURem = FFI.constURem
constSRem = FFI.constSRem
constFRem = FFI.constFRem
constAnd = FFI.constAnd
constOr = FFI.constOr
constXor = FFI.constXor
constShl = FFI.constShl
constLShr = FFI.constLShr
constAShr = FFI.constAShr
constTrunc = FFI.constTrunc
constSExt = FFI.constSExt
constZExt = FFI.constZExt
constFPTrunc = FFI.constFPTrunc
constFPExt = FFI.constFPExt
constUIToFP = FFI.constUIToFP
constSIToFP = FFI.constSIToFP
constFPToUI = FFI.constFPToUI
constFPToSI = FFI.constFPToSI
constPtrToInt = FFI.constPtrToInt
constIntToPtr = FFI.constIntToPtr
constBitCast = FFI.constBitCast
constSExtOrBitCast = FFI.constSExtOrBitCast
constZExtOrBitCast = FFI.constZExtOrBitCast
constTruncOrBitCast = FFI.constTruncOrBitCast
constPointerCast = FFI.constPointerCast
constFPCast = FFI.constFPCast
constSelect = FFI.constSelect
constExtractElement = FFI.constExtractElement
constInsertElement = FFI.constInsertElement
constShuffleVector = FFI.constShuffleVector
blockAddress = FFI.blockAddress
getGlobalParent = FFI.getGlobalParent
getFirstGlobal = FFI.getFirstGlobal
getLastGlobal = FFI.getLastGlobal
getNextGlobal = FFI.getNextGlobal
getPreviousGlobal = FFI.getPreviousGlobal
deleteGlobal = FFI.deleteGlobal
setInitializer = FFI.setInitializer
getInitializer = FFI.getInitializer
getFirstFunction = FFI.getFirstFunction
getLastFunction = FFI.getLastFunction
getNextFunction = FFI.getNextFunction
getPreviousFunction = FFI.getPreviousFunction
deleteFunction = FFI.deleteFunction
getParamParent = FFI.getParamParent
getFirstParam = FFI.getFirstParam
getLastParam = FFI.getLastParam
getNextParam = FFI.getNextParam
getPreviousParam = FFI.getPreviousParam
basicBlockAsValue = FFI.basicBlockAsValue
valueAsBasicBlock = FFI.valueAsBasicBlock
getBasicBlockParent = FFI.getBasicBlockParent
getBasicBlockTerminator = FFI.getBasicBlockTerminator
getFirstBasicBlock = FFI.getFirstBasicBlock
getLastBasicBlock = FFI.getLastBasicBlock
getNextBasicBlock = FFI.getNextBasicBlock
getPreviousBasicBlock = FFI.getPreviousBasicBlock
getEntryBasicBlock = FFI.getEntryBasicBlock
deleteBasicBlock = FFI.deleteBasicBlock
removeBasicBlockFromParent = FFI.removeBasicBlockFromParent
moveBasicBlockBefore = FFI.moveBasicBlockBefore
moveBasicBlockAfter = FFI.moveBasicBlockAfter
getFirstInstruction = FFI.getFirstInstruction
getLastInstruction = FFI.getLastInstruction
getInstructionParent = FFI.getInstructionParent
getNextInstruction = FFI.getNextInstruction
getPreviousInstruction = FFI.getPreviousInstruction
instructionEraseFromParent = FFI.instructionEraseFromParent
getSwitchDefaultDest = FFI.getSwitchDefaultDest
createBuilderInContext = FFI.createBuilderInContext
createBuilder = FFI.createBuilder
positionBuilder = FFI.positionBuilder
positionBefore = FFI.positionBefore
positionAtEnd = FFI.positionAtEnd
getInsertBlock = FFI.getInsertBlock
clearInsertionPosition = FFI.clearInsertionPosition
insertIntoBuilder = FFI.insertIntoBuilder
getCurrentDebugLocation = FFI.getCurrentDebugLocation
setCurrentDebugLocation = FFI.setCurrentDebugLocation
setInstDebugLocation = FFI.setInstDebugLocation
buildRetVoid = FFI.buildRetVoid
buildRet = FFI.buildRet
buildBr = FFI.buildBr
buildCondBr = FFI.buildCondBr
buildResume = FFI.buildResume
buildUnreachable = FFI.buildUnreachable
addCase = FFI.addCase
addDestination = FFI.addDestination
addClause = FFI.addClause
disposeMemoryBuffer = FFI.disposeMemoryBuffer
getGlobalPassRegistry = FFI.getGlobalPassRegistry
createPassManager = FFI.createPassManager
createFunctionPassManagerForModule = FFI.createFunctionPassManagerForModule
createFunctionPassManager = FFI.createFunctionPassManager
disposePassManager = FFI.disposePassManager
buildFree = FFI.buildFree
buildStore = FFI.buildStore