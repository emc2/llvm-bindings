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

-- | This module defines a class for monad which carry an LLVM builder.
module Control.Monad.LLVM.LLVMBuilder.Class(
       MonadLLVMBuilder(..)
       ) where

import Control.Monad.Trans
import LLVM.Core(ValueRef, TypeRef, BasicBlockRef,
                 Opcode, IntPredicate, RealPredicate)

-- | Class for monads that carry LLVM context information
class MonadIO m => MonadLLVMBuilder m where
  positionBuilder :: BasicBlockRef -> ValueRef -> m ()
  positionBefore :: ValueRef -> m ()
  positionAtEnd :: BasicBlockRef -> m ()
  getInsertBlock :: m BasicBlockRef
  clearInsertionPosition :: m ()
  insertIntoBuilder :: ValueRef -> m ()
  insertIntoBuilderWithName :: ValueRef -> String -> m ()
  getCurrentDebugLocation :: m ValueRef
  setCurrentDebugLocation :: ValueRef -> m ()
  setInstDebugLocation :: ValueRef -> m ()
  buildRetVoid :: m ValueRef
  buildRet :: ValueRef -> m ValueRef
  buildAggregateRet :: [ValueRef] -> m ValueRef
  buildBr :: BasicBlockRef -> m ValueRef
  buildCondBr :: ValueRef -> BasicBlockRef -> BasicBlockRef -> m ValueRef
  buildSwitch :: Integral n => ValueRef -> BasicBlockRef -> n -> m ValueRef
  buildIndirectBr :: Integral n => ValueRef -> n -> m ValueRef
  buildInvoke :: ValueRef -> [ValueRef] -> BasicBlockRef ->
                 BasicBlockRef -> String -> m ValueRef
  buildLandingPad :: Integral n =>
                     TypeRef -> ValueRef -> n -> String -> m ValueRef
  buildResume :: ValueRef -> m ValueRef
  buildUnreachable :: m ValueRef
  buildAdd :: ValueRef -> ValueRef -> String -> m ValueRef
  buildNSWAdd :: ValueRef -> ValueRef -> String -> m ValueRef
  buildNUWAdd :: ValueRef -> ValueRef -> String -> m ValueRef
  buildFAdd :: ValueRef -> ValueRef -> String -> m ValueRef
  buildSub :: ValueRef -> ValueRef -> String -> m ValueRef
  buildNSWSub :: ValueRef -> ValueRef -> String -> m ValueRef
  buildNUWSub :: ValueRef -> ValueRef -> String -> m ValueRef
  buildFSub :: ValueRef -> ValueRef -> String -> m ValueRef
  buildMul :: ValueRef -> ValueRef -> String -> m ValueRef
  buildNSWMul :: ValueRef -> ValueRef -> String -> m ValueRef
  buildNUWMul :: ValueRef -> ValueRef -> String -> m ValueRef
  buildFMul :: ValueRef -> ValueRef -> String -> m ValueRef
  buildUDiv :: ValueRef -> ValueRef -> String -> m ValueRef
  buildSDiv :: ValueRef -> ValueRef -> String -> m ValueRef
  buildExactSDiv :: ValueRef -> ValueRef -> String -> m ValueRef
  buildFDiv :: ValueRef -> ValueRef -> String -> m ValueRef
  buildURem :: ValueRef -> ValueRef -> String -> m ValueRef
  buildSRem :: ValueRef -> ValueRef -> String -> m ValueRef
  buildFRem :: ValueRef -> ValueRef -> String -> m ValueRef
  buildShl :: ValueRef -> ValueRef -> String -> m ValueRef
  buildAShr :: ValueRef -> ValueRef -> String -> m ValueRef
  buildLShr :: ValueRef -> ValueRef -> String -> m ValueRef
  buildAnd :: ValueRef -> ValueRef -> String -> m ValueRef
  buildOr :: ValueRef -> ValueRef -> String -> m ValueRef
  buildXor :: ValueRef -> ValueRef -> String -> m ValueRef
  buildBinOp :: Opcode -> ValueRef -> ValueRef -> String -> m ValueRef
  buildNeg :: ValueRef -> String -> m ValueRef
  buildNSWNeg :: ValueRef -> String -> m ValueRef
  buildNUWNeg :: ValueRef -> String -> m ValueRef
  buildFNeg :: ValueRef -> String -> m ValueRef
  buildNot :: ValueRef -> String -> m ValueRef
  buildMalloc :: TypeRef -> String -> m ValueRef
  buildArrayMalloc :: TypeRef -> ValueRef -> String -> m ValueRef
  buildAlloca :: TypeRef -> String -> m ValueRef
  buildArrayAlloca :: TypeRef -> ValueRef -> String -> m ValueRef
  buildFree :: ValueRef -> m ValueRef
  buildLoad :: ValueRef -> String -> m ValueRef

  buildStore :: ValueRef
             -- ^ Value
             -> ValueRef
             -- ^ Pointer value
             -> m ValueRef
             -- ^ Store instruction

  buildGEP :: ValueRef
           -- ^ Pointer value
           -> [ValueRef]
           -- ^ Indexes
           -> String
           -- ^ Name
           -> m ValueRef

  buildInBoundsGEP :: ValueRef
                   -- ^ Pointer value
                   -> [ValueRef]
                   -- ^ Indexes
                   -> String
                   -- ^ Name
                   -> m ValueRef

  buildStructGEP :: Integral i => ValueRef -> i -> String -> m ValueRef
  buildGlobalString :: String -> String -> m ValueRef
  buildGlobalStringPtr :: String -> String -> m ValueRef
  buildTrunc :: ValueRef -> TypeRef -> String -> m ValueRef
  buildZExt :: ValueRef -> TypeRef -> String -> m ValueRef
  buildSExt :: ValueRef -> TypeRef -> String -> m ValueRef
  buildFPToUI :: ValueRef -> TypeRef -> String -> m ValueRef
  buildFPToSI :: ValueRef -> TypeRef -> String -> m ValueRef
  buildUIToFP :: ValueRef -> TypeRef -> String -> m ValueRef
  buildSIToFP :: ValueRef -> TypeRef -> String -> m ValueRef
  buildFPTrunc :: ValueRef -> TypeRef -> String -> m ValueRef
  buildFPExt :: ValueRef -> TypeRef -> String -> m ValueRef
  buildPtrToInt :: ValueRef -> TypeRef -> String -> m ValueRef
  buildIntToPtr :: ValueRef -> TypeRef -> String -> m ValueRef
  buildBitCast :: ValueRef -> TypeRef -> String -> m ValueRef
  buildZExtOrBitCast :: ValueRef -> TypeRef -> String -> m ValueRef
  buildSExtOrBitCast :: ValueRef -> TypeRef -> String -> m ValueRef
  buildTruncOrBitCast :: ValueRef -> TypeRef -> String -> m ValueRef
  buildCast :: Opcode -> ValueRef -> TypeRef -> String -> m ValueRef
  buildPointerCast :: ValueRef -> TypeRef -> String -> m ValueRef
  buildIntCast :: ValueRef -> TypeRef -> String -> m ValueRef
  buildFPCast :: ValueRef -> TypeRef -> String -> m ValueRef
  buildICmp :: IntPredicate -> ValueRef -> ValueRef -> String -> m ValueRef
  buildFCmp :: RealPredicate -> ValueRef -> ValueRef -> String -> m ValueRef
  buildPhi :: TypeRef -> String -> m ValueRef
  buildCall :: ValueRef -> [ValueRef] -> String -> m ValueRef
  buildSelect :: ValueRef -> ValueRef -> ValueRef -> String -> m ValueRef
  buildVAArg :: ValueRef -> TypeRef -> String -> m ValueRef
  buildExtractElement :: ValueRef -> ValueRef -> String -> m ValueRef

  buildInsertElement :: ValueRef -> ValueRef -> ValueRef -> String ->
                        m ValueRef

  buildShuffleVector :: ValueRef -> ValueRef -> ValueRef -> String ->
                        m ValueRef

  buildExtractValue :: Integral n => ValueRef -> n -> String -> m ValueRef

  buildInsertValue :: Integral n =>
                      ValueRef -> ValueRef -> n -> String -> m ValueRef

  buildIsNull :: ValueRef -> String -> m ValueRef
  buildIsNotNull :: ValueRef -> String -> m ValueRef
  buildPtrDiff :: ValueRef -> ValueRef -> String -> m ValueRef
