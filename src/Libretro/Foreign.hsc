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
  :: Dyn (CUInt -> IO (Ptr ()))

foreign import ccall "dynamic" retro_get_memory_size
  :: Dyn (CUInt -> IO CSize)

-- Callbacks ----------------------------------------------------------------

type Wrap a = a -> IO (FunPtr a)

type RetroEnvironmentT = CUInt -> Ptr () -> IO CInt
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

type RetroInputStateT = CUInt -> CUInt -> CUInt -> CUInt -> IO CShort
foreign import ccall "wrapper" retro_input_state_t
  :: Wrap RetroInputStateT

-- lots are missing

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
  { retroGameGeometryBaseWidth :: Word
  , retroGameGeometryBaseHeight :: Word
  , retroGameGeometryMaxWidth :: Word
  , retroGameGeometryMaxHeight :: Word
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
