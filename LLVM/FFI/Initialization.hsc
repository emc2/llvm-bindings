{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.Initialization(
       initializeCore,
       initializeTransformUtils,
       initializeScalarOpts,
       initializeVectorization,
       initializeInstCombine,
       initializeInstrumentation,
       initializeAnalysis,
       initializeIPA,
       initializeCodeGen,
       initializeTarget
       ) where

import LLVM.FFI.Core

foreign import ccall unsafe "LLVMInitializeCore"
  initializeCore :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMInitializeTransformUtils"
  initializeTransformUtils :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMInitializeScalarOpts"
  initializeScalarOpts :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMInitializeVectorization"
  initializeVectorization :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMInitializeInstCombine"
  initializeInstCombine :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMInitializeInstrumentation"
  initializeInstrumentation :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMInitializeAnalysis"
  initializeAnalysis :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMInitializeIPA"
  initializeIPA :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMInitializeCodeGen"
  initializeCodeGen :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMInitializeTarget"
  initializeTarget :: PassRegistryRef -> IO ()
