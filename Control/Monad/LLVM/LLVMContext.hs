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

-- | This module defines an implementation of the LLVMContext monad class.
module Control.Monad.LLVM.LLVMContext(
       MonadLLVMContext(..),
       LLVMContextT,
       LLVMContext,
       runWithContext,
       runWithNewContext,
       runWithGlobalContext
       ) where

import Control.Applicative
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.LLVM.LLVMContext.Class
import Control.Monad.LLVM.LLVMModule.Class
import Control.Monad.Writer.Class
import Control.Monad.Reader
import Control.Monad.State

import qualified LLVM.Core as LLVM

newtype LLVMContextT m a =
  LLVMContextT { unpackLLVMContextT :: ReaderT LLVM.ContextRef m a }
type LLVMContext a = LLVMContextT IO a

-- | Run an LLVMContextT with a given context
runWithContext :: MonadIO m => LLVMContextT m a -> LLVM.ContextRef -> m a
runWithContext m = runReaderT (unpackLLVMContextT m)

-- | Create a fresh context and use it to run an LLVMContextT.  The
-- created context is disposed after running the LLVMContextT, so the
-- result must not depend on its continued existence.
runWithNewContext :: MonadIO m => LLVMContextT m a -> m a
runWithNewContext m =
  do
    ctx <- liftIO LLVM.contextCreate
    res <- runReaderT (unpackLLVMContextT m) ctx
    liftIO (LLVM.contextDispose ctx)
    return res

-- | Get the global context and use it to run an LLVMContextT
runWithGlobalContext :: MonadIO m => LLVMContextT m a -> m a
runWithGlobalContext m =
  liftIO LLVM.getGlobalContext >>= runReaderT (unpackLLVMContextT m)

getMDKindIDInContext' :: (MonadIO m, Num n) =>
                         String -> (ReaderT LLVM.ContextRef m) n
getMDKindIDInContext' name =
  do
    ctx <- ask
    liftIO (LLVM.getMDKindIDInContext ctx name)

moduleCreateWithNameInContext' :: MonadIO m => String ->
                                  (ReaderT LLVM.ContextRef m) LLVM.ModuleRef
moduleCreateWithNameInContext' name =
  ask >>= (liftIO . LLVM.moduleCreateWithNameInContext name)

int1TypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
int1TypeInContext' = ask >>= liftIO . LLVM.int1TypeInContext

int8TypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
int8TypeInContext' = ask >>= liftIO . LLVM.int8TypeInContext

int16TypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
int16TypeInContext' = ask >>= liftIO . LLVM.int16TypeInContext

int32TypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
int32TypeInContext' = ask >>= liftIO . LLVM.int32TypeInContext

int64TypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
int64TypeInContext' = ask >>= liftIO . LLVM.int64TypeInContext

floatTypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
floatTypeInContext' = ask >>= liftIO . LLVM.floatTypeInContext

doubleTypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
doubleTypeInContext' = ask >>= liftIO . LLVM.doubleTypeInContext

x86FP80TypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
x86FP80TypeInContext' = ask >>= liftIO . LLVM.x86FP80TypeInContext

fp128TypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
fp128TypeInContext' = ask >>= liftIO . LLVM.fp128TypeInContext

ppcFP128TypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
ppcFP128TypeInContext' = ask >>= liftIO . LLVM.ppcFP128TypeInContext

voidTypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
voidTypeInContext' = ask >>= liftIO . LLVM.voidTypeInContext

labelTypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
labelTypeInContext' = ask >>= liftIO . LLVM.labelTypeInContext

x86MMXTypeInContext' :: MonadIO m => (ReaderT LLVM.ContextRef m) LLVM.TypeRef
x86MMXTypeInContext' = ask >>= liftIO . LLVM.x86MMXTypeInContext

intTypeInContext' :: (MonadIO m, Integral n) => n ->
                     (ReaderT LLVM.ContextRef m) LLVM.TypeRef
intTypeInContext' n =
  do
    ctx <- ask
    liftIO (LLVM.intTypeInContext ctx n)

structTypeInContext' :: MonadIO m => [LLVM.TypeRef] -> Bool ->
                        (ReaderT LLVM.ContextRef m) LLVM.TypeRef
structTypeInContext' fields packed =
  do
    ctx <- ask
    liftIO (LLVM.structTypeInContext ctx fields packed)

structCreateNamed' :: MonadIO m => String ->
                      (ReaderT LLVM.ContextRef m) LLVM.TypeRef
structCreateNamed' name =
  do
    ctx <- ask
    liftIO (LLVM.structCreateNamed ctx name)

constStringInContext' :: MonadIO m => String -> Bool ->
                         (ReaderT LLVM.ContextRef m) LLVM.ValueRef
constStringInContext' name nullterm =
  do
    ctx <- ask
    liftIO (LLVM.constStringInContext ctx name nullterm)

constStructInContext' :: MonadIO m => [LLVM.ValueRef] -> Bool ->
                         (ReaderT LLVM.ContextRef m) LLVM.ValueRef
constStructInContext' fields packed =
  do
    ctx <- ask
    liftIO (LLVM.constStructInContext ctx fields packed)

mdStringInContext' :: MonadIO m => String ->
                      (ReaderT LLVM.ContextRef m) LLVM.ValueRef
mdStringInContext' name =
  do
    ctx <- ask
    liftIO (LLVM.mdStringInContext ctx name)

mdNodeInContext' :: MonadIO m => [LLVM.ValueRef] ->
                    (ReaderT LLVM.ContextRef m) LLVM.ValueRef
mdNodeInContext' vals =
  do
    ctx <- ask
    liftIO (LLVM.mdNodeInContext ctx vals)

appendBasicBlockInContext' :: MonadIO m => LLVM.ValueRef -> String ->
                              (ReaderT LLVM.ContextRef m) LLVM.BasicBlockRef
appendBasicBlockInContext' func name =
  do
    ctx <- ask
    liftIO (LLVM.appendBasicBlockInContext ctx func name)

insertBasicBlockInContext' :: MonadIO m => LLVM.BasicBlockRef -> String ->
                              (ReaderT LLVM.ContextRef m) LLVM.BasicBlockRef
insertBasicBlockInContext' block name =
  do
    ctx <- ask
    liftIO (LLVM.insertBasicBlockInContext ctx block name)

createBuilderInContext' :: MonadIO m =>
                           (ReaderT LLVM.ContextRef m) LLVM.BuilderRef
createBuilderInContext' = ask >>= liftIO . LLVM.createBuilderInContext

instance MonadIO m => MonadLLVMContext (LLVMContextT m) where
  getMDKindIDInContext = LLVMContextT . getMDKindIDInContext'
  moduleCreateWithNameInContext = LLVMContextT . moduleCreateWithNameInContext'
  int1TypeInContext = LLVMContextT int1TypeInContext'
  int8TypeInContext = LLVMContextT int8TypeInContext'
  int16TypeInContext = LLVMContextT int16TypeInContext'
  int32TypeInContext = LLVMContextT int32TypeInContext'
  int64TypeInContext = LLVMContextT int64TypeInContext'
  intTypeInContext = LLVMContextT . intTypeInContext'
  floatTypeInContext = LLVMContextT floatTypeInContext'
  doubleTypeInContext = LLVMContextT doubleTypeInContext'
  x86FP80TypeInContext = LLVMContextT x86FP80TypeInContext'
  fp128TypeInContext = LLVMContextT fp128TypeInContext'
  ppcFP128TypeInContext = LLVMContextT ppcFP128TypeInContext'
  voidTypeInContext = LLVMContextT voidTypeInContext'
  labelTypeInContext = LLVMContextT labelTypeInContext'
  x86MMXTypeInContext = LLVMContextT x86MMXTypeInContext'
  structTypeInContext fields = LLVMContextT . structTypeInContext' fields
  structCreateNamed = LLVMContextT . structCreateNamed'
  constStringInContext name = LLVMContextT . constStringInContext' name
  constStructInContext fields = LLVMContextT . constStructInContext' fields
  mdStringInContext = LLVMContextT . mdStringInContext'
  mdNodeInContext = LLVMContextT . mdNodeInContext'
  appendBasicBlockInContext block =
    LLVMContextT . appendBasicBlockInContext' block
  insertBasicBlockInContext block =
    LLVMContextT . insertBasicBlockInContext' block
  createBuilderInContext = LLVMContextT createBuilderInContext'

instance Monad m => Monad (LLVMContextT m) where
  return = LLVMContextT . return
  LLVMContextT x >>= f = LLVMContextT $ x >>= unpackLLVMContextT . f

instance MonadIO m => MonadIO (LLVMContextT m) where
  liftIO = LLVMContextT . liftIO

instance MonadTrans LLVMContextT where
  lift = LLVMContextT . lift

instance MonadState s m => MonadState s (LLVMContextT m) where
  get = lift get
  put = lift . put

instance MonadLLVMModule m => MonadLLVMModule (LLVMContextT m) where
  getDataLayout = lift getDataLayout
  setDataLayout = lift . setDataLayout
  getTarget = lift getTarget
  setTarget = lift . setTarget
  dumpModule = lift dumpModule
  getTypeByName = lift . getTypeByName
  getNamedMetadataNumOperands = lift . getNamedMetadataNumOperands
  getNamedMetadataOperands = lift . getNamedMetadataOperands
  addNamedMetadataOperand name = lift . addNamedMetadataOperand name
  addFunction name = lift . addFunction name
  getNamedFunction = lift . getNamedFunction
  getFirstFunction = lift getFirstFunction
  getLastFunction = lift getLastFunction
  getModuleContext = lift getModuleContext
  setModuleInlineAsm = lift . setModuleInlineAsm
  addGlobal name = lift . addGlobal name
  addGlobalInAddressSpace name ty = lift . addGlobalInAddressSpace name ty
  getNamedGlobal = lift . getNamedGlobal
  getFirstGlobal = lift getFirstGlobal
  getLastGlobal = lift getLastGlobal
  addAlias ty val = lift . addAlias ty val

instance (Functor m) => Functor (LLVMContextT m) where
  fmap f  = LLVMContextT . mapReaderT (fmap f) . unpackLLVMContextT

instance (Applicative m) => Applicative (LLVMContextT m) where
  pure = LLVMContextT . pure
  f <*> v = LLVMContextT $ unpackLLVMContextT f <*> unpackLLVMContextT v

instance Alternative m => Alternative (LLVMContextT m) where
  empty = LLVMContextT empty
  m <|> n = LLVMContextT $ unpackLLVMContextT m <|> unpackLLVMContextT n

instance MonadPlus m => MonadPlus (LLVMContextT m) where
  mzero = lift mzero
  m `mplus` n = LLVMContextT $ unpackLLVMContextT m `mplus` unpackLLVMContextT n

instance MonadFix m => MonadFix (LLVMContextT m) where
  mfix f = LLVMContextT $ mfix $ (\a -> unpackLLVMContextT (f a))

instance MonadCont m => MonadCont (LLVMContextT m) where
  callCC f =
    LLVMContextT $ callCC $
      (\c -> unpackLLVMContextT (f (\a -> LLVMContextT $ c a)))

instance (MonadError e m) => MonadError e (LLVMContextT m) where
  throwError = lift . throwError
  m `catchError` h =
    LLVMContextT $ unpackLLVMContextT m `catchError`
                     (\e -> unpackLLVMContextT (h e))

instance MonadWriter w m => MonadWriter w (LLVMContextT m) where
  tell = lift . tell
  listen m = LLVMContextT $ listen (unpackLLVMContextT m)
  pass m = LLVMContextT $ pass (unpackLLVMContextT m)
