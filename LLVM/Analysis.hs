module LLVM.Analysis(
       verifyModule,
       verifyFunction,
       viewFunctionCFG,
       viewFunctionCFGOnly
       ) where

import Foreign.C.String
import Foreign.C.Types(CInt(..))
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr(Ptr)
import Foreign.Storable
import LLVM.FFI.Core(ModuleRef, ValueRef)

import qualified LLVM.FFI.Analysis as FFI
import qualified LLVM.FFI.Core as FFI

action = FFI.fromVerifierFailureAction FFI.ReturnStatus

verifyModule :: ModuleRef -> IO (Bool, String)
verifyModule mod =
  let
    verifyModule' ptr =
      do
        res <- FFI.verifyModule mod action ptr
        msg <- peek ptr
        str <- peekCString msg
        FFI.disposeMessage msg
        return (not (toBool res), str)
  in
    alloca verifyModule'
     
verifyFunction :: ValueRef -> IO Bool
verifyFunction func =
  do
    res <- FFI.verifyFunction func action
    return (res == 1)

viewFunctionCFG = FFI.viewFunctionCFG
viewFunctionCFGOnly = FFI.viewFunctionCFGOnly