module LLVM.BitReader(
       parseBitcode,
       parseBitcodeInContext,
       getBitcodeModule,
       getBitcodeModuleInContext
       ) where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import LLVM.FFI.Core(ModuleRef, MemoryBufferRef, ContextRef)

import qualified LLVM.FFI.BitReader as FFI
import qualified LLVM.FFI.Core as FFI

parseBitcode :: MemoryBufferRef -> IO (Either ModuleRef String)
parseBitcode mbuf =
  let
    parseBitcode' pref sref =
      do
        res <- FFI.parseBitcode mbuf pref sref
        if res == 1
          then do
            out <- peek pref
            return (Left out)
          else do
            msg <- peek sref
            str <- peekCString msg
            FFI.disposeMessage msg
            return (Right str)
  in
    alloca (\ref -> alloca (parseBitcode' ref))

parseBitcodeInContext :: ContextRef -> MemoryBufferRef ->
                         IO (Either ModuleRef String)
parseBitcodeInContext ctx mbuf =
  let
    parseBitcodeInContext' pref sref =
      do
        res <- FFI.parseBitcodeInContext ctx mbuf pref sref
        if res == 1
          then do
            out <- peek pref
            return (Left out)
          else do
            msg <- peek sref
            str <- peekCString msg
            FFI.disposeMessage msg
            return (Right str)
  in
    alloca (\ref -> alloca (parseBitcodeInContext' ref))

getBitcodeModule :: MemoryBufferRef -> IO (Either ModuleRef String)
getBitcodeModule mod =
  let
    getBitcodeModule' pref sref =
      do
        res <- FFI.getBitcodeModule mod pref sref
        if res == 1
          then do
            out <- peek pref
            return (Left out)
          else do
            msg <- peek sref
            str <- peekCString msg
            FFI.disposeMessage msg
            return (Right str)
  in
    alloca (\ref -> alloca (getBitcodeModule' ref))

getBitcodeModuleInContext :: ContextRef -> MemoryBufferRef ->
                             IO (Either ModuleRef String)
getBitcodeModuleInContext ctx mbuf =
  let
    getBitcodeModuleInContext' pref sref =
      do
        res <- FFI.getBitcodeModuleInContext ctx mbuf pref sref
        if res == 1
          then do
            out <- peek pref
            return (Left out)
          else do
            msg <- peek sref
            str <- peekCString msg
            FFI.disposeMessage msg
            return (Right str)
  in
    alloca (\ref -> alloca (getBitcodeModuleInContext' ref))
