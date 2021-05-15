module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Word
import System.Console.Haskeline
import System.Environment
import Text.Printf

import AppM
import Command
import Libretro
import Audio
import Video

makePrompt :: AppM String
makePrompt = do
  core' <- use appCore
  case core' of
    Nothing -> return "no core"
    Just (core, corestate) -> do
      sysinfo <- liftIO $ runRetroM core retroGetSystemInfo
      return $ printf "%s %s (%s)"
        (retroSystemInfoLibraryName sysinfo)
        (retroSystemInfoLibraryVersion sysinfo)
        (formatCoreState corestate)
  where
    formatCoreState :: CoreState -> String
    formatCoreState CoreFresh = "loaded"
    formatCoreState CoreInitialized = "initialized"
    formatCoreState CoreRunning = "running"

main :: IO ()
main = do
  audio <- initAudio
  video <- initVideo
  state <- initAppState audio video
  evalAppM (runInputT defaultSettings inputLoop) state

inputLoop :: InputT AppM ()
inputLoop = do
  printFun <- getExternalPrint
  lift $ appPrint .= lift . printFun
  loop
  where
    loop = do
      prompt <- lift makePrompt
      input <- getInputLine (prompt ++ "> ")

      lift runAppTasks

      case input of
        Nothing -> return ()
        Just l -> lift (runCommand l) >> loop

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

runCommand :: String -> AppM ()
runCommand = parseCommand
  [ cmdLoadcore
  , cmdInfo
  , cmdAvinfo
  , cmdLoadgame
  , cmdRun
  ]

cmdLoadcore :: Command
cmdLoadcore = commandP "loadcore" "<path>" $ do
  path <- stringP
  return $ do
    core <- liftIO (tryIO (loadCore path))
    case core of
      Left err -> output $ printf "IOException: %s" (show err)
      Right core' -> do
        appCore .= Just (core', CoreFresh)
        output $ printf "Loaded %s" path

cmdInfo :: Command
cmdInfo = commandP "info" "" $ do
  return $ withLoadedCore [CoreFresh, CoreInitialized, CoreRunning] $ \core -> do
    av <- liftIO $ runRetroM core retroApiVersion
    output $ printf "libretro API version: %d" av

    sysinfo <- liftIO $ runRetroM core retroGetSystemInfo
    output $ printf "Core name: %s" (retroSystemInfoLibraryName sysinfo)
    output $ printf "Core version: %s" (retroSystemInfoLibraryVersion sysinfo)

cmdAvinfo :: Command
cmdAvinfo = commandP "avinfo" "" $ do
  return $ withLoadedCore [CoreFresh, CoreInitialized, CoreRunning] $ \core -> do
    avInfo <- liftIO $ runRetroM core retroGetSystemAvInfo

    output $ printf "Base dimensions: %s * %s"
      (show (retroGameGeometryBaseWidth (retroSystemAvInfoGeometry avInfo)))
      (show (retroGameGeometryBaseHeight (retroSystemAvInfoGeometry avInfo)))
    output $ printf "Max dimensions: %s * %s"
      (show (retroGameGeometryMaxWidth (retroSystemAvInfoGeometry avInfo)))
      (show (retroGameGeometryMaxHeight (retroSystemAvInfoGeometry avInfo)))
    output $ printf "Aspect ratio: %s"
      (show (retroGameGeometryAspectRatio (retroSystemAvInfoGeometry avInfo)))

cmdLoadgame :: Command
cmdLoadgame = commandP "loadgame" "<path>" $ do
  path <- stringP
  return $ withLoadedCore [CoreFresh] $ \core -> do
    avInfo <- liftIO $ runRetroM core retroGetSystemAvInfo

    audio <- use appAudio
    video <- use appVideo

    let environmentHandlers = defaultRetroEnvironmentHandlers
          { retroEnvironmentHandlersSetPixelFormat = videoSetPixelFormat video
          }

    liftIO $ runRetroM core $ do
      retroSetEnvironment environmentHandlers
      retroSetVideoRefresh (videoRefresh video)
      retroSetInputPoll (videoInputPoll defaultInputMappings video)
      retroSetInputState (videoInputState video)
      retroSetAudioSample (audioSample audio)
      retroSetAudioSampleBatch (audioSampleBatch audio)
      retroInit

    result <- liftIO $ tryIO $ runRetroM core $ do
      gameInfo <- loadGameInfo path
      retroLoadGame gameInfo

    case result of
      Left err -> output $ printf "IOException: %s" (show err)
      Right () -> appCore . traversed . _2 .= CoreInitialized

cmdRun :: Command
cmdRun = do
  symbolP "run"
  return $ withLoadedCore [CoreInitialized] $ \core -> do
    avInfo <- liftIO $ runRetroM core retroGetSystemAvInfo

    video <- use appVideo
    audio <- use appAudio

    taskQueue <- use appMainTasks

    let runLoop = do
          runRetroM core retroRun
          videoRender video

          shouldClose <- windowShouldClose video
          if shouldClose
            then atomically $ writeTQueue taskQueue $ do
              appCore . traversed . _2 .= CoreInitialized
            else runLoop

    liftIO $ forkIO $ do
      audioResult <- tryIO $
        configureAudio audio (floor (retroSystemTimingSampleRate (retroSystemAvInfoTiming avInfo)))
      case audioResult of
        Left err -> atomically $ writeTQueue taskQueue (output $ printf "IOException: %s" (show err))
        Right () -> return ()

      configureVideo video (retroSystemAvInfoGeometry avInfo)
      runLoop
      deconfigureVideo video
      runRetroM core $ retroDeinit

    appCore . traversed . _2 .= CoreRunning

    output "Game is running"

withLoadedCore :: [CoreState] -> (RetroCore -> AppM ()) -> AppM ()
withLoadedCore states f = do
  core <- use appCore
  case core of
    Nothing -> output "Error: No core loaded"
    Just (core', corestate) -> if corestate `elem` states
                                  then f core'
                                  else output "Error: Invalid core state"

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
