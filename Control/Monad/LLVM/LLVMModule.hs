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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-- | This module defines an implementation of the LLVMModule monad class.
module Control.Monad.LLVM.LLVMModule(
       MonadLLVMModule(..),
       LLVMModuleT,
       LLVMModule,
       runWithModule,
       runWithNewModule,
       runWithNewModuleInContext
       ) where

import Control.Applicative
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.LLVM.LLVMBuilder.Class
import Control.Monad.LLVM.LLVMContext.Class
import Control.Monad.LLVM.LLVMModule.Class
import Control.Monad.Writer.Class
import Control.Monad.Reader
import Control.Monad.State
import Prelude hiding (mod)

import qualified LLVM.Analysis as Analysis
import qualified LLVM.BitWriter as BitWriter
import qualified LLVM.Core as LLVM

newtype LLVMModuleT m a =
  LLVMModuleT { unpackLLVMModuleT :: ReaderT LLVM.ModuleRef m a }
type LLVMModule a = LLVMModuleT IO a

-- | Run an LLVMModuleT with a given module.
runWithModule :: MonadIO m => LLVMModuleT m a -> LLVM.ModuleRef -> m a
runWithModule m = runReaderT (unpackLLVMModuleT m)

-- | Create a new module with a given name and use it to run an
-- LLVMModuleT.  The created module is disposed at the end, so the
-- result value must not depend on its continued existence.
runWithNewModule :: MonadIO m => LLVMModuleT m a -> String -> m a
runWithNewModule m name =
  do
    mod <- liftIO (LLVM.moduleCreateWithName name)
    res <- runReaderT (unpackLLVMModuleT m) mod
    liftIO (LLVM.disposeModule mod)
    return res

-- | Create a new module with a given name in the context contained by
-- an LLVMContextRefT and use it to run an LLVMModuleT.  The created
-- module is disposed at the end, so the result value must not depend
-- on its continued existence.
runWithNewModuleInContext :: (MonadLLVMContext m, MonadIO m) =>
                             LLVMModuleT m a -> String -> m a
runWithNewModuleInContext m name =
  do
    mod <- moduleCreateWithNameInContext name
    res <- runReaderT (unpackLLVMModuleT m) mod
    liftIO (LLVM.disposeModule mod)
    return res

getDataLayout' :: MonadIO m => (ReaderT LLVM.ModuleRef m) String
getDataLayout' = ask >>= liftIO . LLVM.getDataLayout

setDataLayout' :: MonadIO m => String -> (ReaderT LLVM.ModuleRef m) ()
setDataLayout' layout =
  do
    mod <- ask
    liftIO (LLVM.setDataLayout mod layout)

getTarget' :: MonadIO m => (ReaderT LLVM.ModuleRef m) String
getTarget' = ask >>= liftIO . LLVM.getTarget

setTarget' :: MonadIO m => String -> (ReaderT LLVM.ModuleRef m) ()
setTarget' name =
  do
    mod <- ask
    liftIO (LLVM.setTarget mod name)

dumpModule' :: MonadIO m => (ReaderT LLVM.ModuleRef m) ()
dumpModule' = ask >>= liftIO . LLVM.dumpModule

getTypeByName' :: MonadIO m => String -> (ReaderT LLVM.ModuleRef m) LLVM.TypeRef
getTypeByName' name =
  do
    mod <- ask
    liftIO (LLVM.getTypeByName mod name)

getNamedMetadataNumOperands' :: (MonadIO m, Integral n) => String ->
                                (ReaderT LLVM.ModuleRef m) n
getNamedMetadataNumOperands' name =
  do
    mod <- ask
    liftIO (LLVM.getNamedMetadataNumOperands mod name)

getNamedMetadataOperands' :: MonadIO m => String ->
                             (ReaderT LLVM.ModuleRef m) [LLVM.ValueRef]
getNamedMetadataOperands' name =
  do
    mod <- ask
    liftIO (LLVM.getNamedMetadataOperands mod name)

addNamedMetadataOperand' :: MonadIO m => String -> LLVM.ValueRef ->
                            (ReaderT LLVM.ModuleRef m) ()
addNamedMetadataOperand' name md =
  do
    mod <- ask
    liftIO (LLVM.addNamedMetadataOperand mod name md)

addFunction' :: MonadIO m => String -> LLVM.TypeRef ->
                (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
addFunction' name ty =
  do
    mod <- ask
    liftIO (LLVM.addFunction mod name ty)

getNamedFunction' :: MonadIO m => String ->
                     (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
getNamedFunction' name =
  do
    mod <- ask
    liftIO (LLVM.getNamedFunction mod name)

getFirstFunction' :: MonadIO m => (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
getFirstFunction' = ask >>= liftIO . LLVM.getFirstFunction

getLastFunction' :: MonadIO m => (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
getLastFunction' = ask >>= liftIO . LLVM.getLastFunction

getModuleContext' :: MonadIO m => (ReaderT LLVM.ModuleRef m) LLVM.ContextRef
getModuleContext' = ask >>= liftIO . LLVM.getModuleContext

setModuleInlineAsm' :: MonadIO m => String -> (ReaderT LLVM.ModuleRef m) ()
setModuleInlineAsm' name =
  do
    mod <- ask
    liftIO (LLVM.setModuleInlineAsm mod name)

addGlobal' :: MonadIO m => LLVM.TypeRef -> String ->
              (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
addGlobal' ty name =
  do
    mod <- ask
    liftIO (LLVM.addGlobal mod ty name)

addGlobalInAddressSpace' :: (MonadIO m, Integral n) => LLVM.TypeRef -> String ->
                            n -> (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
addGlobalInAddressSpace' ty name space =
  do
    mod <- ask
    liftIO (LLVM.addGlobalInAddressSpace mod ty name space)

getNamedGlobal' :: MonadIO m => String ->
                   (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
getNamedGlobal' name =
  do
    mod <- ask
    liftIO (LLVM.getNamedGlobal mod name)

getFirstGlobal' :: MonadIO m => (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
getFirstGlobal' = ask >>= liftIO . LLVM.getFirstGlobal

getLastGlobal' :: MonadIO m => (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
getLastGlobal' = ask >>= liftIO . LLVM.getLastGlobal

addAlias' :: MonadIO m => LLVM.TypeRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
addAlias' ty val name =
  do
    mod <- ask
    liftIO (LLVM.addAlias mod ty val name)

verifyModule' :: MonadIO m => (ReaderT LLVM.ModuleRef m) String
verifyModule' = ask >>= liftIO . Analysis.verifyModule

writeBitcodeToFile' :: MonadIO m => String ->
                   (ReaderT LLVM.ModuleRef m) LLVM.ValueRef
writeBitcodeToFile' name =
  do
    mod <- ask
    liftIO (BitWriter.writeBitcodeToFile mod name)

instance MonadIO m => MonadLLVMModule (LLVMModuleT m) where
  getDataLayout = LLVMModuleT getDataLayout'
  setDataLayout = LLVMModuleT . setDataLayout'
  getTarget = LLVMModuleT getTarget'
  setTarget = LLVMModuleT . setTarget'
  dumpModule = LLVMModuleT dumpModule'
  getTypeByName = LLVMModuleT . getTypeByName'
  getNamedMetadataNumOperands = LLVMModuleT . getNamedMetadataNumOperands'
  getNamedMetadataOperands = LLVMModuleT . getNamedMetadataOperands'
  addNamedMetadataOperand name = LLVMModuleT . addNamedMetadataOperand' name
  addFunction name = LLVMModuleT . addFunction' name
  getNamedFunction = LLVMModuleT . getNamedFunction'
  getFirstFunction = LLVMModuleT getFirstFunction'
  getLastFunction = LLVMModuleT getLastFunction'
  getModuleContext = LLVMModuleT getModuleContext'
  setModuleInlineAsm = LLVMModuleT . setModuleInlineAsm'
  addGlobal name = LLVMModuleT . addGlobal' name
  addGlobalInAddressSpace name ty =
    LLVMModuleT . addGlobalInAddressSpace' name ty
  getNamedGlobal = LLVMModuleT . getNamedGlobal'
  getFirstGlobal = LLVMModuleT getFirstGlobal'
  getLastGlobal = LLVMModuleT getLastGlobal'
  addAlias ty val = LLVMModuleT . addAlias' ty val
  verifyModule = LLVMModuleT verifyModule'
  writeBitcodeToFile = LLVMModuleT . writeBitcodeToFile'

instance Monad m => Monad (LLVMModuleT m) where
  return = LLVMModuleT . return
  LLVMModuleT x >>= f = LLVMModuleT $ x >>= unpackLLVMModuleT . f

instance MonadIO m => MonadIO (LLVMModuleT m) where
  liftIO = LLVMModuleT . liftIO

instance MonadTrans LLVMModuleT where
  lift = LLVMModuleT . lift

instance MonadState s m => MonadState s (LLVMModuleT m) where
  get = lift get
  put = lift . put

instance MonadLLVMContext m => MonadLLVMContext (LLVMModuleT m) where
  getMDKindIDInContext = lift . getMDKindIDInContext
  moduleCreateWithNameInContext = lift . moduleCreateWithNameInContext
  int1TypeInContext = lift int1TypeInContext
  int8TypeInContext = lift int8TypeInContext
  int16TypeInContext = lift int16TypeInContext
  int32TypeInContext = lift int32TypeInContext
  int64TypeInContext = lift int64TypeInContext
  intTypeInContext = lift . intTypeInContext
  floatTypeInContext = lift floatTypeInContext
  doubleTypeInContext = lift doubleTypeInContext
  x86FP80TypeInContext = lift x86FP80TypeInContext
  fp128TypeInContext = lift fp128TypeInContext
  ppcFP128TypeInContext = lift ppcFP128TypeInContext
  voidTypeInContext = lift voidTypeInContext
  labelTypeInContext = lift labelTypeInContext
  x86MMXTypeInContext = lift x86MMXTypeInContext
  structTypeInContext fields = lift . structTypeInContext fields
  structCreateNamed = lift . structCreateNamed
  constStringInContext name = lift . constStringInContext name
  constStructInContext fields = lift . constStructInContext fields
  mdStringInContext = lift . mdStringInContext
  mdNodeInContext = lift . mdNodeInContext
  appendBasicBlockInContext block = lift . appendBasicBlockInContext block
  insertBasicBlockInContext block = lift . insertBasicBlockInContext block
  createBuilderInContext = lift createBuilderInContext

instance MonadLLVMBuilder m => MonadLLVMBuilder (LLVMModuleT m) where
  positionBuilder block = lift . positionBuilder block
  positionBefore = lift . positionBefore
  positionAtEnd = lift . positionAtEnd
  getInsertBlock = lift getInsertBlock
  clearInsertionPosition = lift clearInsertionPosition
  insertIntoBuilder =  lift . insertIntoBuilder
  insertIntoBuilderWithName instr =
    lift . insertIntoBuilderWithName instr
  getCurrentDebugLocation = lift getCurrentDebugLocation
  setCurrentDebugLocation = lift . setCurrentDebugLocation
  setInstDebugLocation = lift . setInstDebugLocation
  buildRetVoid = lift buildRetVoid
  buildRet = lift . buildRet
  buildAggregateRet = lift . buildAggregateRet
  buildBr = lift . buildBr
  buildCondBr test true = lift . buildCondBr test true
  buildSwitch test def = lift . buildSwitch test def
  buildIndirectBr addr = lift . buildIndirectBr addr
  buildInvoke func args ret catchblk =
    lift . buildInvoke func args ret catchblk
  buildLandingPad ty pers nclauses =
    lift . buildLandingPad ty pers nclauses
  buildResume = lift . buildResume
  buildUnreachable = lift buildUnreachable
  buildAdd left right = lift . buildAdd left right
  buildNSWAdd left right = lift . buildNSWAdd left right
  buildNUWAdd left right = lift . buildNUWAdd left right
  buildFAdd left right = lift . buildFAdd left right
  buildSub left right = lift . buildSub left right
  buildNSWSub left right = lift . buildNSWSub left right
  buildNUWSub left right = lift . buildNUWSub left right
  buildFSub left right = lift . buildFSub left right
  buildMul left right = lift . buildMul left right
  buildNSWMul left right = lift . buildNSWMul left right
  buildNUWMul left right = lift . buildNUWMul left right
  buildFMul left right = lift . buildFMul left right
  buildUDiv left right = lift . buildUDiv left right
  buildSDiv left right = lift . buildSDiv left right
  buildExactSDiv left right = lift . buildExactSDiv left right
  buildFDiv left right = lift . buildFDiv left right
  buildURem left right = lift . buildURem left right
  buildSRem left right = lift . buildSRem left right
  buildFRem left right = lift . buildFRem left right
  buildShl left right = lift . buildShl left right
  buildAShr left right = lift . buildAShr left right
  buildLShr left right = lift . buildLShr left right
  buildAnd left right = lift . buildAnd left right
  buildOr left right = lift . buildOr left right
  buildXor left right = lift . buildXor left right
  buildBinOp op left right = lift . buildBinOp op left right
  buildNeg val = lift . buildNeg val
  buildNSWNeg val = lift . buildNSWNeg val
  buildNUWNeg val = lift . buildNUWNeg val
  buildFNeg val = lift . buildFNeg val
  buildNot val = lift . buildNot val
  buildMalloc ty = lift . buildMalloc ty
  buildArrayMalloc ty nelems = lift . buildArrayMalloc ty nelems
  buildAlloca ty = lift . buildAlloca ty
  buildArrayAlloca ty nelems = lift . buildArrayAlloca ty nelems
  buildFree = lift . buildFree
  buildLoad addr = lift . buildLoad addr
  buildStore val = lift . buildStore val
  buildGEP addr indexes = lift . buildGEP addr indexes
  buildInBoundsGEP addr indexes = lift . buildInBoundsGEP addr indexes
  buildStructGEP addr index = lift . buildStructGEP addr index
  buildGlobalString str = lift . buildGlobalString str
  buildGlobalStringPtr str = lift . buildGlobalStringPtr str
  buildTrunc val ty = lift . buildTrunc val ty
  buildZExt val ty = lift . buildZExt val ty
  buildSExt val ty = lift . buildSExt val ty
  buildFPToUI val ty = lift . buildFPToUI val ty
  buildFPToSI val ty = lift . buildFPToSI val ty
  buildUIToFP val ty = lift . buildUIToFP val ty
  buildSIToFP val ty = lift . buildSIToFP val ty
  buildFPTrunc val ty = lift . buildFPTrunc val ty
  buildFPExt val ty = lift . buildFPExt val ty
  buildIntToPtr val ty = lift . buildIntToPtr val ty
  buildPtrToInt val ty = lift . buildPtrToInt val ty
  buildBitCast val ty = lift . buildBitCast val ty
  buildZExtOrBitCast val ty = lift . buildZExtOrBitCast val ty
  buildSExtOrBitCast val ty = lift . buildSExtOrBitCast val ty
  buildTruncOrBitCast val ty = lift . buildTruncOrBitCast val ty
  buildCast op val ty = lift . buildCast op val ty
  buildPointerCast val ty = lift . buildPointerCast val ty
  buildIntCast val ty = lift . buildIntCast val ty
  buildFPCast val ty = lift . buildFPCast val ty
  buildICmp op left right = lift . buildICmp op left right
  buildFCmp op left right = lift . buildFCmp op left right
  buildPhi ty = lift . buildPhi ty
  buildCall func args = lift . buildCall func args
  buildSelect test true false = lift . buildSelect test true false
  buildVAArg val ty = lift . buildVAArg val ty
  buildExtractElement val index = lift . buildExtractElement val index
  buildInsertElement val elemval index =
    lift . buildInsertElement val elemval index
  buildShuffleVector v1 v2 mask = lift . buildShuffleVector v1 v2 mask
  buildExtractValue val index = lift . buildExtractValue val index
  buildInsertValue val elemval index =
    lift . buildInsertValue val elemval index
  buildIsNull val = lift . buildIsNull val
  buildIsNotNull val = lift . buildIsNotNull val
  buildPtrDiff left right = lift . buildPtrDiff left right

instance Functor m => Functor (LLVMModuleT m) where
  fmap f  = LLVMModuleT . mapReaderT (fmap f) . unpackLLVMModuleT

instance Applicative m => Applicative (LLVMModuleT m) where
  pure = LLVMModuleT . pure
  f <*> v = LLVMModuleT $ unpackLLVMModuleT f <*> unpackLLVMModuleT v

instance Alternative m => Alternative (LLVMModuleT m) where
  empty = LLVMModuleT empty
  m <|> n = LLVMModuleT $ unpackLLVMModuleT m <|> unpackLLVMModuleT n

instance MonadPlus m => MonadPlus (LLVMModuleT m) where
  mzero = lift mzero
  m `mplus` n = LLVMModuleT $ unpackLLVMModuleT m `mplus` unpackLLVMModuleT n

instance MonadFix m => MonadFix (LLVMModuleT m) where
  mfix f = LLVMModuleT $ mfix $ (\a -> unpackLLVMModuleT (f a))

instance MonadCont m => MonadCont (LLVMModuleT m) where
  callCC f =
    LLVMModuleT $ callCC $
      (\c -> unpackLLVMModuleT (f (\a -> LLVMModuleT $ c a)))

instance MonadError e m => MonadError e (LLVMModuleT m) where
  throwError = lift . throwError
  m `catchError` h =
    LLVMModuleT $ unpackLLVMModuleT m `catchError`
                    (\e -> unpackLLVMModuleT (h e))

instance MonadWriter w m => MonadWriter w (LLVMModuleT m) where
  tell = lift . tell
  listen m = LLVMModuleT $ listen (unpackLLVMModuleT m)
  pass m = LLVMModuleT $ pass (unpackLLVMModuleT m)
