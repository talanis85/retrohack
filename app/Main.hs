module Main where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Graphics.UI.GLFW as GLFW
import System.Environment
import System.Posix.DynamicLinker
import System.IO
import System.IO.Error
import Text.Printf

import Libretro

printLog :: String -> IO ()
printLog = putStrLn

main :: IO ()
main = do
  args <- getArgs

  -- Initialize GLFW
  glfwInitSuccess <- GLFW.init
  when (not glfwInitSuccess) $ ioError $ userError "Could not initialize GLFW"

  -- Load the libretro core
  withCore (args !! 0) defaultRetroEnvironment $ do
    av <- retroApiVersion
    liftIO $ printLog $ printf "libretro API version: %d" av

    sysinfo <- retroGetSystemInfo
    liftIO $ printLog $ show sysinfo

    retroSetEnvironment defaultRetroEnvironment
    retroSetVideoRefresh videoRefresh
    retroSetInputPoll inputPoll
    retroSetInputState inputState
    retroSetAudioSample audioSample
    retroSetAudioSampleBatch audioSampleBatch

    liftIO $ printLog "Done installing callbacks"

    retroInit

    liftIO $ printLog "Done initializing"

    gameInfo <- loadGameInfo (args !! 1)
    retroLoadGame gameInfo

    {-
    gameInfo <- loadGameInfo core (args !! 1)
    retroLoadGame core gameInfo

    retroSetEnvironment core environmentCallback
    -}

{-
environmentCallback :: LibretroEnvironment -> Ptr () -> IO CInt
environmentCallback env dat
  | key == c'RETRO_ENVIRONMENT_GET_LOG_INTERFACE = do
      logfun <- default_retro_log_printf
      let logcb = C'retro_log_callback logfun
      poke (castPtr dat) logcb
      return 1
  | otherwise = return 0
-}

loadGameInfo :: FilePath -> RetroM RetroGameInfo
loadGameInfo fp = do
  bs <- liftIO $ BS.readFile fp
  sysinfo <- retroGetSystemInfo

  let infoData = if retroSystemInfoNeedFullPath sysinfo
        then BS.unpack bs
        else []

  return RetroGameInfo
    { retroGameInfoPath = fp
    , retroGameInfoData = infoData
    , retroGameInfoMeta = ""
    }

videoRefresh :: PixelData -> Word -> Word -> Word -> RetroM ()
videoRefresh dat width height pitch = return ()

inputPoll :: RetroM ()
inputPoll = return ()

inputState :: Word -> Word -> Word -> Word -> RetroM Int16
inputState port device index id = return 0

audioSample :: Int16 -> Int16 -> RetroM ()
audioSample left right = return ()

audioSampleBatch :: [Int16] -> RetroM Word
audioSampleBatch dat = return 0
