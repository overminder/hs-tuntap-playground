module Network.TunTap.Omni where

import Control.Applicative
import System.Posix
import System.Posix.IO
import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C
import Foreign.C.String
import Data.Bits
import Data.Word
import Network.Socket

-- XXX
iFNAMSIZ = 16

foreign import ccall safe "os.h tunAlloc" tunAlloc_c
  :: Int -> CString -> IO Int

foreign import ccall safe "os.h tunBringUp" tunBringUp_c
  :: CString -> IO Int

foreign import ccall safe "os.h tunSetIpAndMask" tunSetIpAndMask_c
  :: CString -> Word32 -> Word32 -> IO Int

foreign import ccall safe "os.h tunGetMtu" tunGetMtu_c
  :: CString -> Ptr CInt -> IO Int

data Device
  = Device {
    devType :: DeviceType,
    devFd :: Int,
    devName :: String
  }
  deriving (Show, Eq, Ord)

data DeviceType
  = TUN | TAP
  deriving (Show, Eq, Ord)

new :: Maybe String -> DeviceType -> Bool -> IO Device
new mbName ty isNoPi = withName_c $ \ name_c -> do
  fd <- tunAlloc_c isTun name_c
  nameGot <- peekCString name_c
  return $ Device ty fd nameGot
 where
  withName_c = maybe (allocaBytes iFNAMSIZ) withCString mbName
  isTun = case ty of
    TUN -> 1
    TAP -> 0

bringUp :: Device -> IO ()
bringUp (Device {..}) = withCString devName $ \ name_c -> do
  0 <- tunBringUp_c name_c
  return ()

toHandle :: Device -> IO Handle
toHandle = fdToHandle . fromIntegral . devFd

setIpAndMask :: HostAddress -> HostAddress -> Device -> IO ()
setIpAndMask ip mask (Device {..}) = withCString devName $ \ name_c -> do
  0 <- tunSetIpAndMask_c name_c ip mask
  return ()


getMtu :: Device -> IO Int
getMtu (Device {..}) = withCString devName $ \ name_c -> do
  alloca $ \ mtuPtr -> do
    0 <- tunGetMtu_c name_c mtuPtr
    fromIntegral <$> peek mtuPtr

