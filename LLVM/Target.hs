module LLVM.Target(
       ByteOrdering,
       createTargetData,
       addTargetData,
       addTargetLibraryInfo,
       copyStringRepOfTargetData,
       byteOrder,
       pointerSize,
       pointerType,
       intPtrType,
       sizeOfTypeInBits,
       storeSizeOfType,
       abiSizeOfType,
       abiAlignmentOfType,
       callFrameAlignmentOfType,
       preferredAlignmentOfType,
       preferredAlignmentOfGlobal,
       elementAtOffset,
       offsetOfElement,
       disposeTargetData
       ) where

import Data.Word
import Foreign.C.String
import LLVM.FFI.Core
import LLVM.FFI.Target(TargetDataRef)

import qualified LLVM.FFI.Target as FFI

type ByteOrdering = FFI.ByteOrdering

createTargetData :: String -> IO TargetDataRef
createTargetData str = withCString str FFI.createTargetData

copyStringRepOfTargetData :: TargetDataRef -> IO String
copyStringRepOfTargetData t = FFI.copyStringRepOfTargetData t >>= peekCString

byteOrder :: TargetDataRef -> ByteOrdering
byteOrder = FFI.toByteOrdering . FFI.byteOrder

pointerSize :: Num n => TargetDataRef -> n
pointerSize = fromIntegral . FFI.pointerSize

sizeOfTypeInBits :: Num n => TargetDataRef -> TypeRef -> n
sizeOfTypeInBits targ = fromIntegral . FFI.sizeOfTypeInBits targ

storeSizeOfType :: Num n => TargetDataRef -> TypeRef -> n
storeSizeOfType targ = fromIntegral . FFI.storeSizeOfType targ

abiSizeOfType :: Num n => TargetDataRef -> TypeRef -> n
abiSizeOfType targ = fromIntegral . FFI.abiSizeOfType targ

abiAlignmentOfType :: Num n => TargetDataRef -> TypeRef -> n
abiAlignmentOfType targ = fromIntegral . FFI.abiAlignmentOfType targ

callFrameAlignmentOfType :: Num n => TargetDataRef -> TypeRef -> n
callFrameAlignmentOfType targ =
  fromIntegral . FFI.callFrameAlignmentOfType targ

preferredAlignmentOfType :: Num n => TargetDataRef -> TypeRef -> n
preferredAlignmentOfType targ =
  fromIntegral . FFI.preferredAlignmentOfType targ

preferredAlignmentOfGlobal :: Num n => TargetDataRef -> ValueRef -> n
preferredAlignmentOfGlobal targ =
  fromIntegral . FFI.preferredAlignmentOfGlobal targ

elementAtOffset :: (Integral m, Num n) => TargetDataRef -> TypeRef -> m -> n
elementAtOffset targ ty =
  fromIntegral . FFI.elementAtOffset targ ty . fromIntegral

offsetOfElement :: (Integral m, Num n) => TargetDataRef -> TypeRef -> m -> n
offsetOfElement targ ty =
  fromIntegral . FFI.offsetOfElement targ ty . fromIntegral

addTargetData = FFI.addTargetData
addTargetLibraryInfo = FFI.addTargetLibraryInfo
intPtrType = FFI.intPtrType
disposeTargetData = FFI.disposeTargetData