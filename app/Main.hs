module Main where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as BS
import Data.Word
import System.Environment
import System.Posix.DynamicLinker
import System.IO
import System.IO.Error
import Text.Printf

import Log
import Libretro
import Video

main :: IO ()
main = do
  args <- getArgs

  video <- initVideo

  withCore (args !! 0) $ do
    av <- retroApiVersion
    liftIO $ printLog $ printf "libretro API version: %d" av

    sysinfo <- retroGetSystemInfo
    liftIO $ printLog $ printf "Core name: %s" (retroSystemInfoLibraryName sysinfo)
    liftIO $ printLog $ printf "Core version: %s" (retroSystemInfoLibraryVersion sysinfo)

    let environmentHandlers = defaultRetroEnvironmentHandlers
          { retroEnvironmentHandlersSetPixelFormat = videoSetPixelFormat video
          }

    retroSetEnvironment environmentHandlers
    retroSetVideoRefresh (videoRefresh video)
    retroSetInputPoll inputPoll
    retroSetInputState inputState
    retroSetAudioSample audioSample
    retroSetAudioSampleBatch audioSampleBatch

    retroInit

    gameInfo <- loadGameInfo (args !! 1)
    retroLoadGame gameInfo

    avInfo <- retroGetSystemAvInfo

    liftIO $ printLog $ "Game geometry:"
    liftIO $ printLog $ printf "  base dimensions: %s * %s"
      (show (retroGameGeometryBaseWidth (retroSystemAvInfoGeometry avInfo)))
      (show (retroGameGeometryBaseHeight (retroSystemAvInfoGeometry avInfo)))
    liftIO $ printLog $ printf "  max dimensions: %s * %s"
      (show (retroGameGeometryMaxWidth (retroSystemAvInfoGeometry avInfo)))
      (show (retroGameGeometryMaxHeight (retroSystemAvInfoGeometry avInfo)))
    liftIO $ printLog $ printf "  aspect ratio: %s"
      (show (retroGameGeometryAspectRatio (retroSystemAvInfoGeometry avInfo)))

    liftIO $ configureVideo video (retroSystemAvInfoGeometry avInfo)

    let loop = do
          retroRun
          liftIO $ videoRender video

          shouldClose <- liftIO $ windowShouldClose video
          if shouldClose then return () else loop

    loop

loadGameInfo :: FilePath -> RetroM RetroGameInfo
loadGameInfo fp = do
  bs <- liftIO $ BS.readFile fp
  sysinfo <- retroGetSystemInfo

  let infoData = if retroSystemInfoNeedFullPath sysinfo
        then []
        else BS.unpack bs

  return RetroGameInfo
    { retroGameInfoPath = fp
    , retroGameInfoData = infoData
    , retroGameInfoMeta = ""
    }

inputPoll :: RetroM ()
inputPoll = return ()

inputState :: Word32 -> Word32 -> Word32 -> Word32 -> RetroM Int16
inputState port device index id = return 0

audioSample :: Int16 -> Int16 -> RetroM ()
audioSample left right = return ()

audioSampleBatch :: [Int16] -> RetroM Word64
audioSampleBatch dat = return 0
