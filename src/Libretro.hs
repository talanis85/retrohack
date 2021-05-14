{-# LANGUAGE RecordWildCards #-}

module Libretro
  ( RetroCore
  , RetroM
  , withCore

  , RetroEnvironment (..)
  , defaultRetroEnvironment

  , retroApiVersion
  , retroDeinit
  , retroGetSystemInfo
  , retroInit
  , retroLoadGame
  , retroSetEnvironment
  , retroSetAudioSample
  , retroSetAudioSampleBatch
  , retroSetInputPoll
  , retroSetInputState
  , retroSetVideoRefresh

  , RetroGameInfo (..)
  , RetroSystemInfo (..)

  , PixelData
  , RetroVideoRefresh
  , RetroInputPoll
  , RetroInputState
  , RetroAudioSample
  , RetroAudioSampleBatch

  -- * Re-exports
  , Int16
  ) where

import Control.Monad.Reader
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Posix.DynamicLinker

import Libretro.Helpers
import Libretro.Foreign

type RetroM = ReaderT RetroCore IO

data RetroCore = RetroCore
  { _retroApiVersion :: IO CUInt
  , _retroCheatReset :: IO ()
  , _retroCheatSet :: CUInt -> CInt -> CString -> IO ()
  , _retroDeinit :: IO ()
  , _retroGetMemoryData :: CUInt -> IO (Ptr ())
  , _retroGetMemorySize :: CUInt -> IO CSize
  , _retroGetRegion :: IO CUInt
  , _retroGetSystemInfo :: Ptr RetroSystemInfo -> IO ()
  , _retroGetSystemAvInfo :: Ptr RetroSystemAvInfo -> IO ()
  , _retroInit :: IO ()
  , _retroLoadGame :: Ptr RetroGameInfo -> IO CInt
  , _retroLoadGameSpecial :: CUInt -> Ptr RetroGameInfo -> CSize -> IO CInt
  , _retroReset :: IO ()
  , _retroRun :: IO ()
  , _retroSerialize :: Ptr () -> CSize -> IO CInt
  , _retroSerializeSize :: IO CSize
  , _retroSetAudioSample :: FunPtr RetroAudioSampleT -> IO ()
  , _retroSetAudioSampleBatch :: FunPtr RetroAudioSampleBatchT -> IO ()
  , _retroSetControllerPortDevice :: CUInt -> CUInt -> IO ()
  , _retroSetEnvironment :: FunPtr RetroEnvironmentT -> IO ()
  , _retroSetInputPoll :: FunPtr RetroInputPollT -> IO ()
  , _retroSetInputState :: FunPtr RetroInputStateT -> IO ()
  , _retroSetVideoRefresh :: FunPtr RetroVideoRefreshT -> IO ()
  , _retroUnloadGame :: IO ()
  , _retroUnserialize :: Ptr () -> CSize -> IO CInt
  }

loadCore :: FilePath -> IO RetroCore
loadCore fp = do
  dl <- dlopen fp [RTLD_LAZY, RTLD_LOCAL]

  _retroApiVersion              <- retro_api_version <$> dlsym dl "retro_api_version"
  _retroCheatReset              <- retro_cheat_reset <$> dlsym dl "retro_cheat_reset"
  _retroCheatSet                <- retro_cheat_set <$> dlsym dl "retro_cheat_set"
  _retroDeinit                  <- retro_deinit <$> dlsym dl "retro_deinit"
  _retroGetMemoryData           <- retro_get_memory_data <$> dlsym dl "retro_get_memory_data"
  _retroGetMemorySize           <- retro_get_memory_size <$> dlsym dl "retro_get_memory_size"
  _retroGetRegion               <- retro_get_region <$> dlsym dl "retro_get_region"
  _retroGetSystemInfo           <- retro_get_system_info <$> dlsym dl "retro_get_system_info"
  _retroGetSystemAvInfo         <- retro_get_system_av_info <$> dlsym dl "retro_get_system_av_info"
  _retroInit                    <- retro_init <$> dlsym dl "retro_init"
  _retroLoadGame                <- retro_load_game <$> dlsym dl "retro_load_game"
  _retroLoadGameSpecial         <- retro_load_game_special <$> dlsym dl "retro_load_game_special"
  _retroReset                   <- retro_reset <$> dlsym dl "retro_reset"
  _retroRun                     <- retro_run <$> dlsym dl "retro_run"
  _retroSerialize               <- retro_serialize <$> dlsym dl "retro_serialize"
  _retroSerializeSize           <- retro_serialize_size <$> dlsym dl "retro_serialize_size"
  _retroSetAudioSample          <- retro_set_audio_sample <$> dlsym dl "retro_set_audio_sample"
  _retroSetAudioSampleBatch     <- retro_set_audio_sample_batch <$> dlsym dl "retro_set_audio_sample_batch"
  _retroSetControllerPortDevice <- retro_set_controller_port_device <$> dlsym dl "retro_set_controller_port_device"
  _retroSetEnvironment          <- retro_set_environment <$> dlsym dl "retro_set_environment"
  _retroSetInputPoll            <- retro_set_input_poll <$> dlsym dl "retro_set_input_poll"
  _retroSetInputState           <- retro_set_input_state <$> dlsym dl "retro_set_input_state"
  _retroSetVideoRefresh         <- retro_set_video_refresh <$> dlsym dl "retro_set_video_refresh"
  _retroUnloadGame              <- retro_unload_game <$> dlsym dl "retro_unload_game"
  _retroUnserialize             <- retro_unserialize <$> dlsym dl "retro_unserialize"

  return $ RetroCore { .. }

type PixelData = Ptr Word8

type RetroVideoRefresh = PixelData -> Word -> Word -> Word -> RetroM ()

makeRetroVideoRefresh :: RetroCore -> RetroVideoRefresh -> RetroVideoRefreshT
makeRetroVideoRefresh core f = \dat width height pitch -> do
  runReaderT (f (castPtr dat) (fromIntegral width) (fromIntegral height) (fromIntegral pitch)) core

type RetroInputPoll = RetroM ()

makeRetroInputPoll :: RetroCore -> RetroInputPoll -> RetroInputPollT
makeRetroInputPoll core f = do
  runReaderT f core

type RetroInputState = Word -> Word -> Word -> Word -> RetroM Int16

makeRetroInputState :: RetroCore -> RetroInputState -> RetroInputStateT
makeRetroInputState core f = \port device index id -> do
  fromIntegral <$>
    runReaderT (f (fromIntegral port) (fromIntegral device) (fromIntegral index) (fromIntegral id)) core

type RetroAudioSample = Int16 -> Int16 -> RetroM ()

makeRetroAudioSample :: RetroCore -> RetroAudioSample -> RetroAudioSampleT
makeRetroAudioSample core f = \left right -> do
  runReaderT (f (fromIntegral left) (fromIntegral right)) core

type RetroAudioSampleBatch = [Int16] -> RetroM Word

makeRetroAudioSampleBatch :: RetroCore -> RetroAudioSampleBatch -> RetroAudioSampleBatchT
makeRetroAudioSampleBatch core f = \dat frames -> do
  values <- peekArray (fromIntegral frames) dat
  fromIntegral <$> runReaderT (f (fromIntegral <$> values)) core

withCore :: FilePath -> RetroEnvironment -> RetroM a -> IO a
withCore fp env action = do
  core <- loadCore fp
  r <- runReaderT action core
  return r

makeRetroEnvironmentCallback :: RetroCore -> RetroEnvironment -> RetroEnvironmentT
makeRetroEnvironmentCallback core env key dat
  | key == 27 = do
      let logfun = default_retro_log_printf
      let logcb = RetroLogCallback
            { retroLogCallbackLog = logfun
            }
      poke (castPtr dat) logcb
      return 1
  | otherwise = return 0
  where
    runGetter field = do
      x <- runReaderT (field env) core
      poke (castPtr dat) x
      return 1
    runSetter field = do
      x <- peek (castPtr dat)
      runReaderT (field env x) core
      return 1

data RetroEnvironment = RetroEnvironment
  { retroEnvironmentGetCanDupe :: RetroM Bool
  }

defaultRetroEnvironment = RetroEnvironment
  { retroEnvironmentGetCanDupe = return False
  }

runCore :: (RetroCore -> IO a) -> RetroM a
runCore f = ReaderT $ \x -> f x

retroApiVersion :: RetroM Int
retroApiVersion = fromIntegral <$> runCore _retroApiVersion

retroDeinit :: RetroM ()
retroDeinit = runCore _retroDeinit

retroGetSystemInfo :: RetroM RetroSystemInfo
retroGetSystemInfo = runCore $ \core -> alloca $ \p -> _retroGetSystemInfo core p >> peek p

retroInit :: RetroM ()
retroInit = runCore _retroInit

retroLoadGame :: RetroGameInfo -> RetroM ()
retroLoadGame gameInfo = runCore $ \core -> with gameInfo $ \p -> _retroLoadGame core p >> return ()

retroSetAudioSample :: RetroAudioSample -> RetroM ()
retroSetAudioSample f = runCore $ \core -> do
  cb <- retro_audio_sample_t (makeRetroAudioSample core f)
  _retroSetAudioSample core cb
  return ()

retroSetAudioSampleBatch :: RetroAudioSampleBatch -> RetroM ()
retroSetAudioSampleBatch f = runCore $ \core -> do
  cb <- retro_audio_sample_batch_t (makeRetroAudioSampleBatch core f)
  _retroSetAudioSampleBatch core cb
  return ()

retroSetEnvironment :: RetroEnvironment -> RetroM ()
retroSetEnvironment env = runCore $ \core -> do
  cb <- retro_environment_t (makeRetroEnvironmentCallback core env)
  _retroSetEnvironment core cb
  return ()

retroSetInputPoll :: RetroInputPoll -> RetroM ()
retroSetInputPoll f = runCore $ \core -> do
  cb <- retro_input_poll_t (makeRetroInputPoll core f)
  _retroSetInputPoll core cb
  return ()

retroSetInputState :: RetroInputState -> RetroM ()
retroSetInputState f = runCore $ \core -> do
  cb <- retro_input_state_t (makeRetroInputState core f)
  _retroSetInputState core cb
  return ()

retroSetVideoRefresh :: RetroVideoRefresh -> RetroM ()
retroSetVideoRefresh f = runCore $ \core -> do
  cb <- retro_video_refresh_t (makeRetroVideoRefresh core f)
  _retroSetVideoRefresh core cb
  return ()

{-
retroDeinit :: RetroM ()
retroDeinit = runCore _retroDeinit

retroGetSystemInfo :: RetroM RetroSystemInfo
retroGetSystemInfo = fmap readSystemInfo $ runCore $ \core -> do
  alloca $ \systemInfoP -> _retroGetSystemInfo core systemInfoP >>= peek

retroGetSystemAvInfo :: RetroM RetroSystemAvInfo
retroGetSystemAvInfo = fmap readSystemAvInfo $ runCore $ \core -> do
  alloca $ \systemAvInfoP -> _retroGetSystemAvInfo core systemAvInfoP >>= peek

retroSetEnvironment :: RetroEnvironment -> RetroM ()
retroSetEnvironment env = do
  core <- ask
  env' <- mk'retro_environment_t (makeRetroEnvironmentCallback core env)
  _retroSetEnvironment core env'
-}
