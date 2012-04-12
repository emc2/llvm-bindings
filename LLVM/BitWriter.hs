module LLVM.BitWriter(
       writeBitcodeToFile
       ) where

import Foreign.C.String
import Foreign.Marshal.Utils
import LLVM.FFI.Core(ModuleRef)

import qualified LLVM.FFI.BitWriter as FFI

writeBitcodeToFile :: ModuleRef -> String -> IO Bool
writeBitcodeToFile mod str =
  withCString str
    (\cstr -> FFI.writeBitcodeToFile mod cstr >>= return . (not . toBool))
