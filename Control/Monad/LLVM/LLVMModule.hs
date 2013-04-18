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
       LLVMModule
       ) where

import Control.Monad.LLVM.LLVMModule.Class
import Control.Monad.Reader
import Control.Monad.State
import Prelude hiding (mod)

import qualified LLVM.Core as LLVM

newtype LLVMModuleT m a =
  LLVMModuleT { unpackLLVMModuleT :: ReaderT LLVM.ModuleRef m a }
type LLVMModule a = LLVMModuleT IO a

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
