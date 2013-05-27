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
import LLVM.Core(ContextRef, ValueRef, TypeRef)

-- | Class for monads that carry LLVM context information
class MonadIO m => MonadLLVMBuilder m where
  positionBuilder :: BasicBlockRef -> ValueRef -> IO ()
  positionBefore :: ValueRef -> IO ()
  positionAtEnd :: BasicBlockRef -> IO ()
  getInsertBlock :: IO BasicBlockRef
  clearInsertionPosition :: IO ()
  insertIntoBuilder :: ValueRef -> IO ()
  insertIntoBuilderWithName :: ValueRef -> String -> IO ()
  getCurrentDebugLocation :: IO ValueRef
  setCurrentDebugLocation :: ValueRef -> IO ()
  setInstDebugLocation :: ValueRef -> IO ()
  buildRetVoid :: IO ValueRef
  buildRet :: ValueRef -> IO ValueRef
  buildAggregateRet :: [ValueRef] -> IO ValueRef
  buildBr :: BasicBlockRef -> IO ValueRef
  buildCondBr :: ValueRef -> BasicBlockRef -> BasicBlockRef -> IO ValueRef
  buildSwitch :: Integral n => ValueRef -> BasicBlockRef -> n -> IO ValueRef
  buildIndirectBr :: Integral n => ValueRef -> n -> IO ValueRef
  buildInvoke :: ValueRef -> [ValueRef] -> BasicBlockRef ->
                 BasicBlockRef -> String -> IO ValueRef
  buildLandingPad :: Integral n =>
                     TypeRef -> ValueRef -> n -> String -> IO ValueRef
  buildResume :: ValueRef -> IO ValueRef
  buildUnreachable :: IO ValueRef
  buildAdd :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildNSWAdd :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildNUWAdd :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildFAdd :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildSub :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildNSWSub :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildNUWSub :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildFSub :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildMul :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildNSWMul :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildNUWMul :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildFMul :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildUDiv :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildSDiv :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildExactSDiv :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildFDiv :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildURem :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildSRem :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildFRem :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildShl :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildAShr :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildLShr :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildAnd :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildOr :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildXor :: ValueRef -> ValueRef -> String -> IO ValueRef
  buildBinOp :: Opcode -> ValueRef -> ValueRef -> String -> IO ValueRef
  buildNeg :: ValueRef -> String -> IO ValueRef
  buildNSWNeg :: ValueRef -> String -> IO ValueRef
  buildNUWNeg :: ValueRef -> String -> IO ValueRef
  buildFNeg :: ValueRef -> String -> IO ValueRef
  buildNot :: ValueRef -> String -> IO ValueRef
  buildMalloc :: TypeRef -> String -> IO ValueRef
  buildArrayMalloc :: TypeRef -> ValueRef -> String -> IO ValueRef
  buildAlloca :: TypeRef -> String -> IO ValueRef
  buildArrayAlloca :: TypeRef -> ValueRef -> String -> IO ValueRef
  buildFree :: ValueRef -> IO ValueRef
  buildLoad :: ValueRef -> String -> IO ValueRef

  buildStore :: ValueRef
             -- ^ Value
             -> ValueRef
             -- ^ Pointer value
             -> IO ValueRef
             -- ^ Store instruction

  buildGEP :: ValueRef
           -- ^ Pointer value
           -> [ValueRef]
           -- ^ Indexes
           -> String
           -- ^ Name
           -> IO ValueRef

  buildInBoundsGEP :: ValueRef
                   -- ^ Pointer value
                   -> [ValueRef]
                   -- ^ Indexes
                   -> String
                   -- ^ Name
                   -> IO ValueRef

  buildStructGEP :: Integral i => ValueRef -> i -> String -> IO ValueRef
  buildGlobalString :: String -> String -> IO ValueRef
  buildGlobalStringPtr :: String -> String -> IO ValueRef
  buildTrunc :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildZExt :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildSExt :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildFPToUI :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildFPToSI :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildUIToFP :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildSIToFP :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildFPTrunc :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildFPExt :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildPtrToInt :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildIntToPtr :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildBitCast :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildZExtOrBitCast :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildSExtOrBitCast :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildTruncOrBitCast :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildCast :: Opcode -> ValueRef -> TypeRef -> String -> IO ValueRef
  buildPointerCast :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildIntCast :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildFPCast :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildICmp :: IntPredicate -> ValueRef -> ValueRef -> String -> IO ValueRef
  buildFCmp :: RealPredicate -> ValueRef -> ValueRef -> String -> IO ValueRef
  buildPhi :: TypeRef -> String -> IO ValueRef
  buildCall :: ValueRef -> [ValueRef] -> String -> IO ValueRef
  buildSelect :: ValueRef -> ValueRef -> ValueRef -> String -> IO ValueRef
  buildVAArg :: ValueRef -> TypeRef -> String -> IO ValueRef
  buildExtractElement :: ValueRef -> ValueRef -> String -> IO ValueRef

  buildInsertElement :: ValueRef -> ValueRef -> ValueRef -> String ->
                        IO ValueRef

  buildShuffleVector :: ValueRef -> ValueRef -> ValueRef -> String ->
                        IO ValueRef

  buildExtractValue :: Integral n => ValueRef -> n -> String -> IO ValueRef

  buildInsertValue :: Integral n =>
                      ValueRef -> ValueRef -> n -> String -> IO ValueRef

  buildIsNull :: ValueRef -> String -> IO ValueRef
  buildIsNotNull :: ValueRef -> String -> IO ValueRef
  buildPtrDiff :: ValueRef -> ValueRef -> String -> IO ValueRef
