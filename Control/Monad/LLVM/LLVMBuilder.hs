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
module Control.Monad.LLVM.LLVMBuilder(
       MonadLLVMBuilder(..),
       LLVMBuilderT,
       LLVMBuilder,
       runWithBuilder,
       runWithNewBuilder,
       runWithNewBuilderInContext
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

import qualified LLVM.Core as LLVM

newtype LLVMBuilderT m a =
  LLVMBuilderT { unpackLLVMBuilderT :: ReaderT LLVM.BuilderRef m a }
type LLVMBuilder a = LLVMBuilderT IO a

-- | Run an LLVMBuilderT with a given builder.
runWithBuilder :: MonadIO m => LLVMBuilderT m a -> LLVM.BuilderRef -> m a
runWithBuilder m = runReaderT (unpackLLVMBuilderT m)

-- | Create a new builder and use it to run an LLVMBuilderT.  The
-- created builder is disposed at the end.
runWithNewBuilder :: MonadIO m => LLVMBuilderT m a -> m a
runWithNewBuilder m =
  do
    builder <- liftIO LLVM.createBuilder
    res <- runReaderT (unpackLLVMBuilderT m) builder
    liftIO (LLVM.disposeBuilder builder)
    return res

-- | Create a new builder in the context contained by an
-- LLVMContextRefT and use it to run an LLVMBuilderT.  The created
-- builer is disposed at the end.
runWithNewBuilderInContext :: (MonadLLVMContext m, MonadIO m) =>
                              LLVMBuilderT m a -> m a
runWithNewBuilderInContext m =
  do
    builder <- createBuilderInContext
    res <- runReaderT (unpackLLVMBuilderT m) builder
    liftIO (LLVM.disposeBuilder builder)
    return res

positionBuilder' :: MonadIO m =>
                    LLVM.BasicBlockRef -> LLVM.ValueRef ->
                    (ReaderT LLVM.BuilderRef m) ()
positionBuilder' block instr =
  do
    builder <- ask
    liftIO (LLVM.positionBuilder builder block instr)

positionBefore' :: MonadIO m => LLVM.ValueRef -> (ReaderT LLVM.BuilderRef m) ()
positionBefore' instr =
  do
    builder <- ask
    liftIO (LLVM.positionBefore builder instr)

positionAtEnd' :: MonadIO m =>
                  LLVM.BasicBlockRef -> (ReaderT LLVM.BuilderRef m) ()
positionAtEnd' block =
  do
    builder <- ask
    liftIO (LLVM.positionAtEnd builder block)

getInsertBlock' :: MonadIO m => (ReaderT LLVM.BuilderRef m) LLVM.BasicBlockRef
getInsertBlock' = ask >>= liftIO . LLVM.getInsertBlock

clearInsertionPosition' :: MonadIO m => (ReaderT LLVM.BuilderRef m) ()
clearInsertionPosition' = ask >>= liftIO . LLVM.clearInsertionPosition

insertIntoBuilder' :: MonadIO m =>
                      LLVM.ValueRef -> (ReaderT LLVM.BuilderRef m) ()
insertIntoBuilder' instr =
  do
    builder <- ask
    liftIO (LLVM.insertIntoBuilder builder instr)

insertIntoBuilderWithName' :: MonadIO m => LLVM.ValueRef -> String ->
                              (ReaderT LLVM.BuilderRef m) ()
insertIntoBuilderWithName' instr name =
  do
    builder <- ask
    liftIO (LLVM.insertIntoBuilderWithName builder instr name)

getCurrentDebugLocation' :: MonadIO m =>
                            (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
getCurrentDebugLocation' = ask >>= liftIO . LLVM.getCurrentDebugLocation

setCurrentDebugLocation' :: MonadIO m =>
                            LLVM.ValueRef -> (ReaderT LLVM.BuilderRef m) ()
setCurrentDebugLocation' loc =
  do
    builder <- ask
    liftIO (LLVM.setCurrentDebugLocation builder loc)

setInstDebugLocation' :: MonadIO m =>
                         LLVM.ValueRef -> (ReaderT LLVM.BuilderRef m) ()
setInstDebugLocation' loc =
  do
    builder <- ask
    liftIO (LLVM.setInstDebugLocation builder loc)

buildRetVoid' :: MonadIO m => (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildRetVoid' = ask >>= liftIO . LLVM.buildRetVoid

buildRet' :: MonadIO m =>
             LLVM.ValueRef -> (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildRet' val =
  do
    builder <- ask
    liftIO (LLVM.buildRet builder val)

buildAggregateRet' :: MonadIO m =>
                      [LLVM.ValueRef] ->
                      (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildAggregateRet' vals =
  do
    builder <- ask
    liftIO (LLVM.buildAggregateRet builder vals)

buildBr' :: MonadIO m =>
            LLVM.BasicBlockRef -> (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildBr' block =
  do
    builder <- ask
    liftIO (LLVM.buildBr builder block)

buildCondBr' :: MonadIO m =>
                LLVM.ValueRef -> LLVM.BasicBlockRef -> LLVM.BasicBlockRef ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildCondBr' test true false =
  do
    builder <- ask
    liftIO (LLVM.buildCondBr builder test true false)

buildSwitch' :: (MonadIO m, Integral n) =>
                LLVM.ValueRef -> LLVM.BasicBlockRef -> n -> 
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildSwitch' test def ncases =
  do
    builder <- ask
    liftIO (LLVM.buildSwitch builder test def ncases)

buildIndirectBr' :: (MonadIO m, Integral n) => LLVM.ValueRef -> n ->
                    (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildIndirectBr' addr ndests =
  do
    builder <- ask
    liftIO (LLVM.buildIndirectBr builder addr ndests)

buildInvoke' :: MonadIO m =>
                LLVM.ValueRef -> [LLVM.ValueRef] -> LLVM.BasicBlockRef ->
                LLVM.BasicBlockRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildInvoke' func args ret catchblk name =
  do
    builder <- ask
    liftIO (LLVM.buildInvoke builder func args ret catchblk name)

buildLandingPad' :: (MonadIO m, Integral n) =>
                    LLVM.TypeRef -> LLVM.ValueRef -> n -> String ->
                    (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildLandingPad' ty pers nclauses name =
  do
    builder <- ask
    liftIO (LLVM.buildLandingPad builder ty pers nclauses name)

buildResume' :: MonadIO m =>
                LLVM.ValueRef -> (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildResume' val =
  do
    builder <- ask
    liftIO (LLVM.buildResume builder val)

buildUnreachable' :: MonadIO m => (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildUnreachable' = ask >>= liftIO . LLVM.buildUnreachable

buildAdd' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildAdd' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildAdd builder left right name)

buildNSWAdd' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNSWAdd' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildNSWAdd builder left right name)

buildNUWAdd' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNUWAdd' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildNUWAdd builder left right name)

buildFAdd' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFAdd' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildFAdd builder left right name)

buildSub' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildSub' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildSub builder left right name)

buildNSWSub' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNSWSub' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildNSWSub builder left right name)

buildNUWSub' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNUWSub' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildNUWSub builder left right name)

buildFSub' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFSub' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildFSub builder left right name)

buildMul' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildMul' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildMul builder left right name)

buildNSWMul' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNSWMul' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildNSWMul builder left right name)

buildNUWMul' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNUWMul' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildNUWMul builder left right name)

buildFMul' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFMul' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildFMul builder left right name)

buildUDiv' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildUDiv' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildUDiv builder left right name)

buildSDiv' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildSDiv' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildSDiv builder left right name)

buildExactSDiv' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
                   (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildExactSDiv' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildExactSDiv builder left right name)

buildFDiv' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFDiv' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildFDiv builder left right name)

buildURem' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildURem' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildURem builder left right name)

buildSRem' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildSRem' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildSRem builder left right name)

buildFRem' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFRem' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildFRem builder left right name)

buildShl' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildShl' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildShl builder left right name)

buildAShr' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildAShr' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildAShr builder left right name)

buildLShr' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildLShr' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildLShr builder left right name)

buildAnd' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildAnd' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildAnd builder left right name)

buildOr' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
            (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildOr' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildOr builder left right name)

buildXor' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
            (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildXor' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildXor builder left right name)

buildBinOp' :: MonadIO m => LLVM.Opcode -> LLVM.ValueRef -> LLVM.ValueRef ->
               String -> (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildBinOp' op left right name =
  do
    builder <- ask
    liftIO (LLVM.buildBinOp builder op left right name)

buildNeg' :: MonadIO m => LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNeg' val name =
  do
    builder <- ask
    liftIO (LLVM.buildNeg builder val name)

buildNSWNeg' :: MonadIO m => LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNSWNeg' val name =
  do
    builder <- ask
    liftIO (LLVM.buildNSWNeg builder val name)

buildNUWNeg' :: MonadIO m => LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNUWNeg' val name =
  do
    builder <- ask
    liftIO (LLVM.buildNUWNeg builder val name)

buildFNeg' :: MonadIO m => LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFNeg' val name =
  do
    builder <- ask
    liftIO (LLVM.buildFNeg builder val name)

buildNot' :: MonadIO m => LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildNot' val name =
  do
    builder <- ask
    liftIO (LLVM.buildNot builder val name)

buildMalloc' :: MonadIO m => LLVM.TypeRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildMalloc' ty name =
  do
    builder <- ask
    liftIO (LLVM.buildMalloc builder ty name)

buildArrayMalloc' :: MonadIO m => LLVM.TypeRef -> LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildArrayMalloc' ty nelems name =
  do
    builder <- ask
    liftIO (LLVM.buildArrayMalloc builder ty nelems name)

buildAlloca' :: MonadIO m => LLVM.TypeRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildAlloca' ty name =
  do
    builder <- ask
    liftIO (LLVM.buildAlloca builder ty name)

buildArrayAlloca' :: MonadIO m => LLVM.TypeRef -> LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildArrayAlloca' ty nelems name =
  do
    builder <- ask
    liftIO (LLVM.buildArrayAlloca builder ty nelems name)

buildFree' :: MonadIO m =>
              LLVM.ValueRef -> (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFree' val =
  do
    builder <- ask
    liftIO (LLVM.buildFree builder val)

buildLoad' :: MonadIO m => LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildLoad' addr name =
  do
    builder <- ask
    liftIO (LLVM.buildLoad builder addr name)

buildStore' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef ->
               (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildStore' val addr =
  do
    builder <- ask
    liftIO (LLVM.buildStore builder val addr)

buildGEP' :: MonadIO m => LLVM.ValueRef -> [LLVM.ValueRef] -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildGEP' addr indexes name =
  do
    builder <- ask
    liftIO (LLVM.buildGEP builder addr indexes name)

buildInBoundsGEP' :: MonadIO m => LLVM.ValueRef -> [LLVM.ValueRef] -> String ->
                     (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildInBoundsGEP' addr indexes name =
  do
    builder <- ask
    liftIO (LLVM.buildInBoundsGEP builder addr indexes name)

buildStructGEP' :: (MonadIO m, Integral n) => LLVM.ValueRef -> n -> String ->
                   (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildStructGEP' addr index name =
  do
    builder <- ask
    liftIO (LLVM.buildStructGEP builder addr index name)

buildGlobalString' :: MonadIO m => String -> String ->
                      (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildGlobalString' str name =
  do
    builder <- ask
    liftIO (LLVM.buildGlobalString builder str name)

buildGlobalStringPtr' :: MonadIO m => String -> String ->
                      (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildGlobalStringPtr' str name =
  do
    builder <- ask
    liftIO (LLVM.buildGlobalStringPtr builder str name)

buildTrunc' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
               (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildTrunc' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildTrunc builder val ty name)

buildZExt' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildZExt' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildZExt builder val ty name)

buildSExt' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildSExt' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildSExt builder val ty name)

buildFPToUI' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFPToUI' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildFPToUI builder val ty name)

buildFPToSI' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFPToSI' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildFPToSI builder val ty name)

buildUIToFP' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildUIToFP' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildUIToFP builder val ty name)

buildSIToFP' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildSIToFP' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildSIToFP builder val ty name)

buildFPTrunc' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
                 (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFPTrunc' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildFPTrunc builder val ty name)

buildFPExt' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
                 (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFPExt' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildFPExt builder val ty name)

buildPtrToInt' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
                 (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildPtrToInt' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildPtrToInt builder val ty name)

buildIntToPtr' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
                 (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildIntToPtr' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildIntToPtr builder val ty name)

buildBitCast' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
               (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildBitCast' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildBitCast builder val ty name)

buildZExtOrBitCast' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
                       (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildZExtOrBitCast' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildZExtOrBitCast builder val ty name)

buildSExtOrBitCast' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
                       (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildSExtOrBitCast' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildSExtOrBitCast builder val ty name)

buildTruncOrBitCast' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
                        (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildTruncOrBitCast' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildTruncOrBitCast builder val ty name)

buildCast' :: MonadIO m => LLVM.Opcode -> LLVM.ValueRef -> LLVM.TypeRef ->
              String -> (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildCast' op val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildCast builder op val ty name)

buildPointerCast' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
                     (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildPointerCast' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildPointerCast builder val ty name)

buildIntCast' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
               (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildIntCast' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildIntCast builder val ty name)

buildFPCast' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
               (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFPCast' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildFPCast builder val ty name)

buildICmp' :: MonadIO m => LLVM.IntPredicate -> LLVM.ValueRef ->
              LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildICmp' op left right name =
  do
    builder <- ask
    liftIO (LLVM.buildICmp builder op left right name)

buildFCmp' :: MonadIO m => LLVM.RealPredicate -> LLVM.ValueRef ->
              LLVM.ValueRef -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildFCmp' op left right name =
  do
    builder <- ask
    liftIO (LLVM.buildFCmp builder op left right name)

buildPhi' :: MonadIO m => LLVM.TypeRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildPhi' ty name =
  do
    builder <- ask
    liftIO (LLVM.buildPhi builder ty name)

buildCall' :: MonadIO m => LLVM.ValueRef -> [LLVM.ValueRef] -> String ->
              (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildCall' func vals name =
  do
    builder <- ask
    liftIO (LLVM.buildCall builder func vals name)

buildSelect' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> LLVM.ValueRef ->
                String -> (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildSelect' test true false name =
  do
    builder <- ask
    liftIO (LLVM.buildSelect builder test true false name)

buildVAArg' :: MonadIO m => LLVM.ValueRef -> LLVM.TypeRef -> String ->
               (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildVAArg' val ty name =
  do
    builder <- ask
    liftIO (LLVM.buildVAArg builder val ty name)

buildExtractElement' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
                        (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildExtractElement' val index name =
  do
    builder <- ask
    liftIO (LLVM.buildExtractElement builder val index name)

buildInsertElement' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef ->
                       LLVM.ValueRef -> String ->
                       (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildInsertElement' val elemval index name =
  do
    builder <- ask
    liftIO (LLVM.buildInsertElement builder val elemval index name)

buildShuffleVector' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef ->
                       LLVM.ValueRef -> String ->
                       (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildShuffleVector' v1 v2 mask name =
  do
    builder <- ask
    liftIO (LLVM.buildShuffleVector builder v1 v2 mask name)

buildExtractValue' :: (MonadIO m, Integral n) => LLVM.ValueRef -> n -> String ->
                      (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildExtractValue' val index name =
  do
    builder <- ask
    liftIO (LLVM.buildExtractValue builder val index name)

buildInsertValue' :: (MonadIO m, Integral n) => LLVM.ValueRef ->
                     LLVM.ValueRef -> n -> String ->
                     (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildInsertValue' val elemval index name =
  do
    builder <- ask
    liftIO (LLVM.buildInsertValue builder val elemval index name)

buildIsNull' :: MonadIO m => LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildIsNull' val name =
  do
    builder <- ask
    liftIO (LLVM.buildIsNull builder val name)

buildIsNotNull' :: MonadIO m => LLVM.ValueRef -> String ->
                (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildIsNotNull' val name =
  do
    builder <- ask
    liftIO (LLVM.buildIsNotNull builder val name)

buildPtrDiff' :: MonadIO m => LLVM.ValueRef -> LLVM.ValueRef -> String ->
             (ReaderT LLVM.BuilderRef m) LLVM.ValueRef
buildPtrDiff' left right name =
  do
    builder <- ask
    liftIO (LLVM.buildPtrDiff builder left right name)

instance MonadIO m => MonadLLVMBuilder (LLVMBuilderT m) where
  positionBuilder block = LLVMBuilderT . positionBuilder' block
  positionBefore = LLVMBuilderT . positionBefore'
  positionAtEnd = LLVMBuilderT . positionAtEnd'
  getInsertBlock = LLVMBuilderT getInsertBlock'
  clearInsertionPosition = LLVMBuilderT clearInsertionPosition'
  insertIntoBuilder =  LLVMBuilderT . insertIntoBuilder'
  insertIntoBuilderWithName instr =
    LLVMBuilderT . insertIntoBuilderWithName' instr
  getCurrentDebugLocation = LLVMBuilderT getCurrentDebugLocation'
  setCurrentDebugLocation = LLVMBuilderT . setCurrentDebugLocation'
  setInstDebugLocation = LLVMBuilderT . setInstDebugLocation'
  buildRetVoid = LLVMBuilderT buildRetVoid'
  buildRet = LLVMBuilderT . buildRet'
  buildAggregateRet = LLVMBuilderT . buildAggregateRet'
  buildBr = LLVMBuilderT . buildBr'
  buildCondBr test true = LLVMBuilderT . buildCondBr' test true
  buildSwitch test def = LLVMBuilderT . buildSwitch' test def
  buildIndirectBr addr = LLVMBuilderT . buildIndirectBr' addr
  buildInvoke func args ret catchblk =
    LLVMBuilderT . buildInvoke' func args ret catchblk
  buildLandingPad ty pers nclauses =
    LLVMBuilderT . buildLandingPad' ty pers nclauses
  buildResume = LLVMBuilderT . buildResume'
  buildUnreachable = LLVMBuilderT buildUnreachable'
  buildAdd left right = LLVMBuilderT . buildAdd' left right
  buildNSWAdd left right = LLVMBuilderT . buildNSWAdd' left right
  buildNUWAdd left right = LLVMBuilderT . buildNUWAdd' left right
  buildFAdd left right = LLVMBuilderT . buildFAdd' left right
  buildSub left right = LLVMBuilderT . buildSub' left right
  buildNSWSub left right = LLVMBuilderT . buildNSWSub' left right
  buildNUWSub left right = LLVMBuilderT . buildNUWSub' left right
  buildFSub left right = LLVMBuilderT . buildFSub' left right
  buildMul left right = LLVMBuilderT . buildMul' left right
  buildNSWMul left right = LLVMBuilderT . buildNSWMul' left right
  buildNUWMul left right = LLVMBuilderT . buildNUWMul' left right
  buildFMul left right = LLVMBuilderT . buildFMul' left right
  buildUDiv left right = LLVMBuilderT . buildUDiv' left right
  buildSDiv left right = LLVMBuilderT . buildSDiv' left right
  buildExactSDiv left right = LLVMBuilderT . buildExactSDiv' left right
  buildFDiv left right = LLVMBuilderT . buildFDiv' left right
  buildURem left right = LLVMBuilderT . buildURem' left right
  buildSRem left right = LLVMBuilderT . buildSRem' left right
  buildFRem left right = LLVMBuilderT . buildFRem' left right
  buildShl left right = LLVMBuilderT . buildShl' left right
  buildAShr left right = LLVMBuilderT . buildAShr' left right
  buildLShr left right = LLVMBuilderT . buildLShr' left right
  buildAnd left right = LLVMBuilderT . buildAnd' left right
  buildOr left right = LLVMBuilderT . buildOr' left right
  buildXor left right = LLVMBuilderT . buildXor' left right
  buildBinOp op left right = LLVMBuilderT . buildBinOp' op left right
  buildNeg val = LLVMBuilderT . buildNeg' val
  buildNSWNeg val = LLVMBuilderT . buildNSWNeg' val
  buildNUWNeg val = LLVMBuilderT . buildNUWNeg' val
  buildFNeg val = LLVMBuilderT . buildFNeg' val
  buildNot val = LLVMBuilderT . buildNot' val
  buildMalloc ty = LLVMBuilderT . buildMalloc' ty
  buildArrayMalloc ty nelems = LLVMBuilderT . buildArrayMalloc' ty nelems
  buildAlloca ty = LLVMBuilderT . buildAlloca' ty
  buildArrayAlloca ty nelems = LLVMBuilderT . buildArrayAlloca' ty nelems
  buildFree = LLVMBuilderT . buildFree'
  buildLoad addr = LLVMBuilderT . buildLoad' addr
  buildStore val = LLVMBuilderT . buildStore' val
  buildGEP addr indexes = LLVMBuilderT . buildGEP' addr indexes
  buildInBoundsGEP addr indexes = LLVMBuilderT . buildInBoundsGEP' addr indexes
  buildStructGEP addr index = LLVMBuilderT . buildStructGEP' addr index
  buildGlobalString str = LLVMBuilderT . buildGlobalString' str
  buildGlobalStringPtr str = LLVMBuilderT . buildGlobalStringPtr' str
  buildTrunc val ty = LLVMBuilderT . buildTrunc' val ty
  buildZExt val ty = LLVMBuilderT . buildZExt' val ty
  buildSExt val ty = LLVMBuilderT . buildSExt' val ty
  buildFPToUI val ty = LLVMBuilderT . buildFPToUI' val ty
  buildFPToSI val ty = LLVMBuilderT . buildFPToSI' val ty
  buildUIToFP val ty = LLVMBuilderT . buildUIToFP' val ty
  buildSIToFP val ty = LLVMBuilderT . buildSIToFP' val ty
  buildFPTrunc val ty = LLVMBuilderT . buildFPTrunc' val ty
  buildFPExt val ty = LLVMBuilderT . buildFPExt' val ty
  buildIntToPtr val ty = LLVMBuilderT . buildIntToPtr' val ty
  buildPtrToInt val ty = LLVMBuilderT . buildPtrToInt' val ty
  buildBitCast val ty = LLVMBuilderT . buildBitCast' val ty
  buildZExtOrBitCast val ty = LLVMBuilderT . buildZExtOrBitCast' val ty
  buildSExtOrBitCast val ty = LLVMBuilderT . buildSExtOrBitCast' val ty
  buildTruncOrBitCast val ty = LLVMBuilderT . buildTruncOrBitCast' val ty
  buildCast op val ty = LLVMBuilderT . buildCast' op val ty
  buildPointerCast val ty = LLVMBuilderT . buildPointerCast' val ty
  buildIntCast val ty = LLVMBuilderT . buildIntCast' val ty
  buildFPCast val ty = LLVMBuilderT . buildFPCast' val ty
  buildICmp op left right = LLVMBuilderT . buildICmp' op left right
  buildFCmp op left right = LLVMBuilderT . buildFCmp' op left right
  buildPhi ty = LLVMBuilderT . buildPhi' ty
  buildCall func args = LLVMBuilderT . buildCall' func args
  buildSelect test true false = LLVMBuilderT . buildSelect' test true false
  buildVAArg val ty = LLVMBuilderT . buildVAArg' val ty
  buildExtractElement val index = LLVMBuilderT . buildExtractElement' val index
  buildInsertElement val elemval index =
    LLVMBuilderT . buildInsertElement' val elemval index
  buildShuffleVector v1 v2 mask = LLVMBuilderT . buildShuffleVector' v1 v2 mask
  buildExtractValue val index = LLVMBuilderT . buildExtractValue' val index
  buildInsertValue val elemval index =
    LLVMBuilderT . buildInsertValue' val elemval index
  buildIsNull val = LLVMBuilderT . buildIsNull' val
  buildIsNotNull val = LLVMBuilderT . buildIsNotNull' val
  buildPtrDiff left right = LLVMBuilderT . buildPtrDiff' left right

instance Monad m => Monad (LLVMBuilderT m) where
  return = LLVMBuilderT . return
  LLVMBuilderT x >>= f = LLVMBuilderT $ x >>= unpackLLVMBuilderT . f

instance MonadIO m => MonadIO (LLVMBuilderT m) where
  liftIO = LLVMBuilderT . liftIO

instance MonadTrans LLVMBuilderT where
  lift = LLVMBuilderT . lift

instance MonadState s m => MonadState s (LLVMBuilderT m) where
  get = lift get
  put = lift . put

instance MonadLLVMContext m => MonadLLVMContext (LLVMBuilderT m) where
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
  parseBitcodeInContext = lift . parseBitcodeInContext
  getBitcodeModuleInContext = lift . getBitcodeModuleInContext
  tbaaRootMetadataInContext = lift . tbaaRootMetadataInContext
  tbaaMetadataInContext name parent = lift . tbaaMetadataInContext name parent
  rangeMetadataInContext bits = lift . rangeMetadataInContext bits
  fpMathMetadataInContext = lift . fpMathMetadataInContext
  loopMetadataInContext = lift loopMetadataInContext
  compileUnitMetadataInContext lang file producer dir main opt flags
                               vers enums types subprogs =
    lift . compileUnitMetadataInContext lang file producer dir main opt
                                        flags vers enums types subprogs
  fileMetadataInContext name = lift . fileMetadataInContext name
  globalVarMetadataInContext compunitmd name dispname linkname filemd
                             lineno typemd islocal defined =
    lift . globalVarMetadataInContext compunitmd name dispname linkname
                                      filemd lineno typemd islocal defined
  subprogramMetadataInContext compunitmd dispname linkname filemd lineno
                              typemd islocal defined basetype flags optimized
                              ref tempparams funcdecl funcvars =
    lift . subprogramMetadataInContext compunitmd dispname linkname filemd
                                       lineno typemd islocal defined basetype
                                       flags optimized ref tempparams funcdecl
                                       funcvars
  blockMetadataInContext subprogrammd lineno colno filemd =
    lift . blockMetadataInContext subprogrammd lineno colno filemd
  basicTypeMetadataInContext compunitmd name filemd lineno size
                             align offset flags =
    lift . basicTypeMetadataInContext compunitmd name filemd lineno
                                      size align offset flags
  derivedTypeMetadataInContext tag compunitmd name filemd lineno
                               size align offset flags =
    lift . derivedTypeMetadataInContext tag compunitmd name filemd lineno
                                        size align offset flags
  compositeTypeMetadataInContext tag compunitmd name filemd lineno
                               size align offset flags derivemd =
    lift . compositeTypeMetadataInContext tag compunitmd name filemd lineno
                                          size align offset flags derivemd
  enumMetadataInContext name = lift . enumMetadataInContext name
  localVarMetadataInContext blockmd name filemd lineno typemd =
    lift . localVarMetadataInContext blockmd name filemd lineno typemd
  argMetadataInContext blockmd name filemd lineno argno typemd =
    lift . argMetadataInContext blockmd name filemd lineno argno typemd
  locationMetadataInContext lineno colno =
    lift . locationMetadataInContext lineno colno

instance MonadLLVMModule m => MonadLLVMModule (LLVMBuilderT m) where
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
  verifyModule = lift verifyModule
  writeBitcodeToFile = lift . writeBitcodeToFile

instance (Functor m) => Functor (LLVMBuilderT m) where
  fmap f  = LLVMBuilderT . mapReaderT (fmap f) . unpackLLVMBuilderT

instance (Applicative m) => Applicative (LLVMBuilderT m) where
  pure = LLVMBuilderT . pure
  f <*> v = LLVMBuilderT $ unpackLLVMBuilderT f <*> unpackLLVMBuilderT v

instance Alternative m => Alternative (LLVMBuilderT m) where
  empty = LLVMBuilderT empty
  m <|> n = LLVMBuilderT $ unpackLLVMBuilderT m <|> unpackLLVMBuilderT n

instance MonadPlus m => MonadPlus (LLVMBuilderT m) where
  mzero = lift mzero
  m `mplus` n = LLVMBuilderT $ unpackLLVMBuilderT m `mplus` unpackLLVMBuilderT n

instance MonadFix m => MonadFix (LLVMBuilderT m) where
  mfix f = LLVMBuilderT $ mfix $ (\a -> unpackLLVMBuilderT (f a))

instance MonadCont m => MonadCont (LLVMBuilderT m) where
  callCC f =
    LLVMBuilderT $ callCC $
      (\c -> unpackLLVMBuilderT (f (\a -> LLVMBuilderT $ c a)))

instance (MonadError e m) => MonadError e (LLVMBuilderT m) where
  throwError = lift . throwError
  m `catchError` h =
    LLVMBuilderT $ unpackLLVMBuilderT m `catchError`
                     (\e -> unpackLLVMBuilderT (h e))

instance MonadWriter w m => MonadWriter w (LLVMBuilderT m) where
  tell = lift . tell
  listen m = LLVMBuilderT $ listen (unpackLLVMBuilderT m)
  pass m = LLVMBuilderT $ pass (unpackLLVMBuilderT m)
