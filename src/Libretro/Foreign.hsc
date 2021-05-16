{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Libretro.Foreign where

import Control.Monad
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include "libretro.h"

-- Opaque types -------------------------------------------------------------

data RetroLogPrintfT

-- Functions ----------------------------------------------------------------

type Dyn a = FunPtr a -> a

foreign import ccall "dynamic" retro_set_environment
  :: Dyn (FunPtr RetroEnvironmentT -> IO ())

foreign import ccall "dynamic" retro_set_video_refresh
  :: Dyn (FunPtr RetroVideoRefreshT -> IO ())

foreign import ccall "dynamic" retro_set_audio_sample
  :: Dyn (FunPtr RetroAudioSampleT -> IO ())

foreign import ccall "dynamic" retro_set_audio_sample_batch
  :: Dyn (FunPtr RetroAudioSampleBatchT -> IO ())

foreign import ccall "dynamic" retro_set_input_poll
  :: Dyn (FunPtr RetroInputPollT -> IO ())

foreign import ccall "dynamic" retro_set_input_state
  :: Dyn (FunPtr RetroInputStateT -> IO ())

foreign import ccall "dynamic" retro_init
  :: Dyn (IO ())

foreign import ccall "dynamic" retro_deinit
  :: Dyn (IO ())

foreign import ccall "dynamic" retro_api_version
  :: Dyn (IO CUInt)

foreign import ccall "dynamic" retro_get_system_info
  :: Dyn (Ptr RetroSystemInfo -> IO ())

foreign import ccall "dynamic" retro_get_system_av_info
  :: Dyn (Ptr RetroSystemAvInfo -> IO ())

foreign import ccall "dynamic" retro_set_controller_port_device
  :: Dyn (CUInt -> CUInt -> IO ())

foreign import ccall "dynamic" retro_reset
  :: Dyn (IO ())

foreign import ccall "dynamic" retro_run
  :: Dyn (IO ())

foreign import ccall "dynamic" retro_serialize_size
  :: Dyn (IO CSize)

foreign import ccall "dynamic" retro_serialize
  :: Dyn (Ptr () -> CSize -> IO CInt)

foreign import ccall "dynamic" retro_unserialize
  :: Dyn (Ptr () -> CSize -> IO CInt)

foreign import ccall "dynamic" retro_cheat_reset
  :: Dyn (IO ())

foreign import ccall "dynamic" retro_cheat_set
  :: Dyn (CUInt -> CInt -> CString -> IO ())

foreign import ccall "dynamic" retro_load_game
  :: Dyn (Ptr RetroGameInfo ->  IO CInt)

foreign import ccall "dynamic" retro_load_game_special
  :: Dyn (CUInt -> Ptr RetroGameInfo -> CSize -> IO CInt)

foreign import ccall "dynamic" retro_unload_game
  :: Dyn (IO ())

foreign import ccall "dynamic" retro_get_region
  :: Dyn (IO CUInt)

foreign import ccall "dynamic" retro_get_memory_data
  :: Dyn (RetroMemory -> IO (Ptr ()))

foreign import ccall "dynamic" retro_get_memory_size
  :: Dyn (RetroMemory -> IO CSize)

-- Callbacks ----------------------------------------------------------------

type Wrap a = a -> IO (FunPtr a)

type RetroEnvironmentT = RetroEnvironment -> Ptr () -> IO CInt
foreign import ccall "wrapper" retro_environment_t
  :: Wrap RetroEnvironmentT

type RetroVideoRefreshT = Ptr () -> CUInt -> CUInt -> CSize -> IO ()
foreign import ccall "wrapper" retro_video_refresh_t
  :: Wrap RetroVideoRefreshT

type RetroAudioSampleT = CShort -> CShort -> IO ()
foreign import ccall "wrapper" retro_audio_sample_t
  :: Wrap RetroAudioSampleT

type RetroAudioSampleBatchT = Ptr CShort -> CSize -> IO CSize
foreign import ccall "wrapper" retro_audio_sample_batch_t
  :: Wrap RetroAudioSampleBatchT

type RetroInputPollT = IO ()
foreign import ccall "wrapper" retro_input_poll_t
  :: Wrap RetroInputPollT

type RetroInputStateT = CUInt -> RetroDevice -> CUInt -> CUInt -> IO CShort
foreign import ccall "wrapper" retro_input_state_t
  :: Wrap RetroInputStateT

-- lots are missing

-- Enums --------------------------------------------------------------------

-- RETRO_ENVIRONMENT_*

newtype RetroEnvironment = RetroEnvironment { getRetroEnvironment :: CInt }
  deriving (Eq, Show, Storable)

#{enum RetroEnvironment, RetroEnvironment
 , retroEnvironmentExperimental = RETRO_ENVIRONMENT_EXPERIMENTAL
 , retroEnvironmentPrivate = RETRO_ENVIRONMENT_PRIVATE
 , retroEnvironmentSetRotation = RETRO_ENVIRONMENT_SET_ROTATION
 , retroEnvironmentGetOverscan = RETRO_ENVIRONMENT_GET_OVERSCAN
 , retroEnvironmentGetCanDupe = RETRO_ENVIRONMENT_GET_CAN_DUPE
 , retroEnvironmentSetMessage = RETRO_ENVIRONMENT_SET_MESSAGE
 , retroEnvironmentShutdown = RETRO_ENVIRONMENT_SHUTDOWN
 , retroEnvironmentSetPerformanceLevel = RETRO_ENVIRONMENT_SET_PERFORMANCE_LEVEL
 , retroEnvironmentGetSystemDirectory = RETRO_ENVIRONMENT_GET_SYSTEM_DIRECTORY
 , retroEnvironmentSetPixelFormat = RETRO_ENVIRONMENT_SET_PIXEL_FORMAT
 , retroEnvironmentSetInputDescriptors = RETRO_ENVIRONMENT_SET_INPUT_DESCRIPTORS
 , retroEnvironmentSetKeyboardCallback = RETRO_ENVIRONMENT_SET_KEYBOARD_CALLBACK
 , retroEnvironmentSetDiskControlInterface = RETRO_ENVIRONMENT_SET_DISK_CONTROL_INTERFACE
 , retroEnvironmentSetHwRender = RETRO_ENVIRONMENT_SET_HW_RENDER
 , retroEnvironmentGetVariable = RETRO_ENVIRONMENT_GET_VARIABLE
 , retroEnvironmentSetVariables = RETRO_ENVIRONMENT_SET_VARIABLES
 , retroEnvironmentGetVariableUpdate = RETRO_ENVIRONMENT_GET_VARIABLE_UPDATE
 , retroEnvironmentSetSupportNoGame = RETRO_ENVIRONMENT_SET_SUPPORT_NO_GAME
 , retroEnvironmentGetLibretroPath = RETRO_ENVIRONMENT_GET_LIBRETRO_PATH
 , retroEnvironmentSetAudioCallback = RETRO_ENVIRONMENT_SET_AUDIO_CALLBACK
 , retroEnvironmentSetFrameTimeCallback = RETRO_ENVIRONMENT_SET_FRAME_TIME_CALLBACK
 , retroEnvironmentGetRumbleInterface = RETRO_ENVIRONMENT_GET_RUMBLE_INTERFACE
 , retroEnvironmentGetInputDeviceCapabilities = RETRO_ENVIRONMENT_GET_INPUT_DEVICE_CAPABILITIES
 , retroEnvironmentGetSensorInterface = RETRO_ENVIRONMENT_GET_SENSOR_INTERFACE
 , retroEnvironmentGetCameraInterface = RETRO_ENVIRONMENT_GET_CAMERA_INTERFACE
 , retroEnvironmentGetLogInterface = RETRO_ENVIRONMENT_GET_LOG_INTERFACE
 , retroEnvironmentGetPerfInterface = RETRO_ENVIRONMENT_GET_PERF_INTERFACE
 , retroEnvironmentGetLocationInterface = RETRO_ENVIRONMENT_GET_LOCATION_INTERFACE
 , retroEnvironmentGetContentDirectory = RETRO_ENVIRONMENT_GET_CONTENT_DIRECTORY
 , retroEnvironmentGetCoreAssetsDirectory = RETRO_ENVIRONMENT_GET_CORE_ASSETS_DIRECTORY
 , retroEnvironmentGetSaveDirectory = RETRO_ENVIRONMENT_GET_SAVE_DIRECTORY
 , retroEnvironmentSetSystemAvInfo = RETRO_ENVIRONMENT_SET_SYSTEM_AV_INFO
 , retroEnvironmentSetProcAddressCallback = RETRO_ENVIRONMENT_SET_PROC_ADDRESS_CALLBACK
 , retroEnvironmentSetSubsystemInfo = RETRO_ENVIRONMENT_SET_SUBSYSTEM_INFO
 , retroEnvironmentSetControllerInfo = RETRO_ENVIRONMENT_SET_CONTROLLER_INFO
 , retroEnvironmentSetMemoryMaps = RETRO_ENVIRONMENT_SET_MEMORY_MAPS
 , retroEnvironmentSetGeometry = RETRO_ENVIRONMENT_SET_GEOMETRY
 , retroEnvironmentGetUsername = RETRO_ENVIRONMENT_GET_USERNAME
 , retroEnvironmentGetLanguage = RETRO_ENVIRONMENT_GET_LANGUAGE
 , retroEnvironmentGetCurrentSoftwareFramebuffer = RETRO_ENVIRONMENT_GET_CURRENT_SOFTWARE_FRAMEBUFFER
 }

-- retro_pixel_format

newtype RetroPixelFormat = RetroPixelFormat { getRetroPixelFormat :: CInt }
  deriving (Eq, Show, Storable)

#{enum RetroPixelFormat, RetroPixelFormat
  , retroPixelFormat0RGB1555 = RETRO_PIXEL_FORMAT_0RGB1555
  , retroPixelFormatXRGB8888 = RETRO_PIXEL_FORMAT_XRGB8888
  , retroPixelFormatRGB565 = RETRO_PIXEL_FORMAT_RGB565
  , retroPixelFormatUnknown = RETRO_PIXEL_FORMAT_UNKNOWN
  }

-- RETRO_DEVICE

newtype RetroDevice = RetroDevice { getRetroDevice :: CUInt }
  deriving (Eq, Show, Ord, Storable)

#{enum RetroDevice, RetroDevice
  , retroDeviceNone = RETRO_DEVICE_NONE
  , retroDeviceJoypad = RETRO_DEVICE_JOYPAD
  , retroDeviceMouse = RETRO_DEVICE_MOUSE
  , retroDeviceKeyboard = RETRO_DEVICE_KEYBOARD
  , retroDeviceLightgun = RETRO_DEVICE_LIGHTGUN
  , retroDeviceAnalog = RETRO_DEVICE_ANALOG
  , retroDevicePointer = RETRO_DEVICE_POINTER
  }

-- RETRO_DEVICE_ID_JOYPAD

newtype RetroDeviceIdJoypad = RetroDeviceIdJoypad { getRetroDeviceIdJoypad :: CUInt }
  deriving (Eq, Enum, Num, Real, Integral, Show, Ord, Storable)

#{enum RetroDeviceIdJoypad, RetroDeviceIdJoypad
  , retroDeviceIdJoypadB = RETRO_DEVICE_ID_JOYPAD_B
  , retroDeviceIdJoypadY = RETRO_DEVICE_ID_JOYPAD_Y
  , retroDeviceIdJoypadSelect = RETRO_DEVICE_ID_JOYPAD_SELECT
  , retroDeviceIdJoypadStart = RETRO_DEVICE_ID_JOYPAD_START
  , retroDeviceIdJoypadUp = RETRO_DEVICE_ID_JOYPAD_UP
  , retroDeviceIdJoypadDown = RETRO_DEVICE_ID_JOYPAD_DOWN
  , retroDeviceIdJoypadLeft = RETRO_DEVICE_ID_JOYPAD_LEFT
  , retroDeviceIdJoypadRight = RETRO_DEVICE_ID_JOYPAD_RIGHT
  , retroDeviceIdJoypadA = RETRO_DEVICE_ID_JOYPAD_A
  , retroDeviceIdJoypadX = RETRO_DEVICE_ID_JOYPAD_X
  , retroDeviceIdJoypadL = RETRO_DEVICE_ID_JOYPAD_L
  , retroDeviceIdJoypadR = RETRO_DEVICE_ID_JOYPAD_R
  , retroDeviceIdJoypadL2 = RETRO_DEVICE_ID_JOYPAD_L2
  , retroDeviceIdJoypadR2 = RETRO_DEVICE_ID_JOYPAD_R2
  , retroDeviceIdJoypadL3 = RETRO_DEVICE_ID_JOYPAD_L3
  , retroDeviceIdJoypadR3 = RETRO_DEVICE_ID_JOYPAD_R3
  }

-- RETRO_MEMORY

newtype RetroMemory = RetroMemory { getRetroMemroy :: CUInt }
  deriving (Eq, Show)

#{enum RetroMemory, RetroMemory
  , retroMemorySaveRam = RETRO_MEMORY_SAVE_RAM
  , retroMemoryRtc = RETRO_MEMORY_RTC
  , retroMemorySystemRam = RETRO_MEMORY_SYSTEM_RAM
  , retroMemoryVideoRam = RETRO_MEMORY_VIDEO_RAM
  , retroMemoryRom = RETRO_MEMORY_ROM
  }

-- Structs ------------------------------------------------------------------

-- some helpers

cBool :: CInt -> Bool
cBool x = x /= 0

peekByteArray :: IO CSize -> IO (Ptr a) -> IO [Word8]
peekByteArray s d = join (liftM2 peekArray (fromIntegral <$> s) (castPtr <$> d))

-- retro_system_info

data RetroSystemInfo = RetroSystemInfo
  { retroSystemInfoLibraryName :: String
  , retroSystemInfoLibraryVersion :: String
  , retroSystemInfoValidExtensions :: String
  , retroSystemInfoNeedFullPath :: Bool
  , retroSystemInfoBlockExtract :: Bool
  } deriving (Eq, Show)

instance Storable RetroSystemInfo where
  sizeOf _ = #{size struct retro_system_info}
  alignment _ = #{alignment struct retro_system_info}
  peek p = RetroSystemInfo
    <$> ( #{peek struct retro_system_info, library_name} p >>= peekCString )
    <*> ( #{peek struct retro_system_info, library_version} p >>= peekCString )
    <*> ( #{peek struct retro_system_info, valid_extensions} p >>= peekCString )
    <*> ( cBool <$> #{peek struct retro_system_info, need_fullpath} p )
    <*> ( cBool <$> #{peek struct retro_system_info, block_extract} p )
  poke p (RetroSystemInfo {..}) = error "Cannot poke RetroSystemInfo"

-- retro_system_av_info

data RetroSystemAvInfo = RetroSystemAvInfo
  { retroSystemAvInfoGeometry :: RetroGameGeometry
  , retroSystemAvInfoTiming :: RetroSystemTiming
  } deriving (Eq, Show)

instance Storable RetroSystemAvInfo where
  sizeOf _ = #{size struct retro_system_av_info}
  alignment _ = #{alignment struct retro_system_av_info}
  peek p = RetroSystemAvInfo
    <$> ( #{peek struct retro_system_av_info, geometry} p )
    <*> ( #{peek struct retro_system_av_info, timing} p )
  poke p (RetroSystemAvInfo {..}) = error "Cannot poke RetroSystemAvInfo"

-- retro_game_geometry

data RetroGameGeometry = RetroGameGeometry
  { retroGameGeometryBaseWidth :: Word32
  , retroGameGeometryBaseHeight :: Word32
  , retroGameGeometryMaxWidth :: Word32
  , retroGameGeometryMaxHeight :: Word32
  , retroGameGeometryAspectRatio :: Float
  } deriving (Eq, Show)

instance Storable RetroGameGeometry where
  sizeOf _ = #{size struct retro_game_geometry}
  alignment _ = #{alignment struct retro_game_geometry}
  peek p = RetroGameGeometry
    <$> ( #{peek struct retro_game_geometry, base_width} p )
    <*> ( #{peek struct retro_game_geometry, base_height} p )
    <*> ( #{peek struct retro_game_geometry, max_width} p )
    <*> ( #{peek struct retro_game_geometry, max_height} p )
    <*> ( #{peek struct retro_game_geometry, aspect_ratio} p )
  poke p (RetroGameGeometry {..}) = error "Cannot poke RetroGameGeometry"

-- retro_system_timing

data RetroSystemTiming = RetroSystemTiming
  { retroSystemTimingFps :: Double
  , retroSystemTimingSampleRate :: Double
  } deriving (Eq, Show)

instance Storable RetroSystemTiming where
  sizeOf _ = #{size struct retro_system_timing}
  alignment _ = #{alignment struct retro_system_timing}
  peek p = RetroSystemTiming
    <$> ( #{peek struct retro_system_timing, fps} p )
    <*> ( #{peek struct retro_system_timing, sample_rate} p )
  poke p (RetroSystemTiming {..}) = error "Cannot poke RetroSystemTiming"

-- retro_game_info

data RetroGameInfo = RetroGameInfo
  { retroGameInfoPath :: String
  , retroGameInfoData :: [Word8]
  , retroGameInfoMeta :: String
  } deriving (Eq, Show)

instance Storable RetroGameInfo where
  sizeOf _ = #{size struct retro_game_info}
  alignment _ = #{alignment struct retro_game_info}
  peek p = RetroGameInfo
    <$> ( #{peek struct retro_game_info, path} p >>= peekCString )
    <*> ( peekByteArray (#{peek struct retro_game_info, data} p) (#{peek struct retro_game_info, size} p) )
    <*> ( #{peek struct retro_game_info, meta} p >>= peekCString )
  poke p (RetroGameInfo {..}) = do
    path <- newCString retroGameInfoPath
    #{poke struct retro_game_info, path} p path
    dat <- newArray retroGameInfoData
    #{poke struct retro_game_info, data} p dat
    #{poke struct retro_game_info, size} p (length retroGameInfoData)
    meta <- newCString retroGameInfoMeta
    #{poke struct retro_game_info, meta} p meta

-- retro_log_callback

data RetroLogCallback = RetroLogCallback
  { retroLogCallbackLog :: FunPtr RetroLogPrintfT
  } deriving (Eq, Show)

instance Storable RetroLogCallback where
  sizeOf _ = #{size struct retro_log_callback}
  alignment _ = #{alignment struct retro_log_callback}
  peek p = RetroLogCallback
    <$> ( #{peek struct retro_log_callback, log} p )
  poke p (RetroLogCallback {..}) = do
    #{poke struct retro_log_callback, log} p retroLogCallbackLog
