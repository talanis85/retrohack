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
import Retrohack.Memory
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
  , cmdPause
  , cmdContinue
  , cmdExec
  , cmdPeek
  , cmdPoke
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
  return $ withLoadedCore [CoreFresh, CoreStopped, CoreRunning, CorePaused] $ \core -> do
    av <- liftIO $ runRetroM core retroApiVersion
    output $ printf "libretro API version: %d" av

    sysinfo <- liftIO $ runRetroM core retroGetSystemInfo
    output $ printf "Core name: %s" (retroSystemInfoLibraryName sysinfo)
    output $ printf "Core version: %s" (retroSystemInfoLibraryVersion sysinfo)

cmdAvinfo :: Command
cmdAvinfo = commandP "avinfo" "" $ do
  return $ withLoadedCore [CoreFresh, CoreStopped, CoreRunning, CorePaused] $ \core -> do
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
      Right () -> appCore . traversed . _2 .= CoreStopped

cmdRun :: Command
cmdRun = commandP "run" "" $ do
  return $ withLoadedCore [CoreStopped] $ \core -> do
    avInfo <- liftIO $ runRetroM core retroGetSystemAvInfo

    video <- use appVideo
    audio <- use appAudio

    taskQueue <- use appMainTasks
    runningVar <- use appCoreRunning

    let runLoop = do
          atomically (takeTMVar runningVar >> putTMVar runningVar ())
          runRetroM core retroRun
          videoRender video

          shouldClose <- windowShouldClose video
          if shouldClose
            then atomically $ writeTQueue taskQueue $ do
              appCore . traversed . _2 .= CoreStopped
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

cmdPause :: Command
cmdPause = commandP "pause" "" $ do
  return $ withLoadedCore [CoreRunning] $ \core -> do
    runningVar <- use appCoreRunning
    liftIO $ atomically $ takeTMVar runningVar
    appCore . traversed . _2 .= CorePaused

cmdContinue :: Command
cmdContinue = commandP "continue" "" $ do
  return $ withLoadedCore [CorePaused] $ \core -> do
    runningVar <- use appCoreRunning
    appCore . traversed . _2 .= CoreRunning
    liftIO $ atomically $ tryPutTMVar runningVar ()
    return ()

cmdExec :: Command
cmdExec = commandP "exec" "<script>" $ do
  path <- stringP
  return $ do
    file <- liftIO $ tryIO $ readFile path
    case file of
      Left err -> output $ printf "IOException: %s" (show err)
      Right file' -> forM_ (lines file') $ runCommand

cmdPeek :: Command
cmdPeek = commandP "peek" "<type> <segment> <address>" $ do
  typ <- typeP
  (segmentname, segment) <- choice
    [ symbolP "sram" >> return ("sram", retroMemorySaveRam)
    , symbolP "rtc" >> return ("rtc", retroMemoryRtc)
    , symbolP "main" >> return ("main", retroMemorySystemRam)
    , symbolP "video" >> return ("video", retroMemoryVideoRam)
    , symbolP "rom" >> return ("rom", retroMemoryRom)
    ]
  address <- fromIntegral <$> addressP
  return $ withLoadedCore [CoreRunning, CorePaused] $ \core -> do
    mdata <- liftIO $ runRetroM core (retroGetMemoryData segment)
    value <- liftIO $ peekMemoryValue snesArch typ mdata address
    case value of
      Nothing -> output $ printf "0x%x = <out_of_bounds> sizeof(%s) = 0x%x (%d)"
                            address (segmentname :: String) (memoryDataSize mdata) (memoryDataSize mdata)
      Just value -> output $ printf "0x%x = %s" address (value ^. formatValue)

cmdPoke :: Command
cmdPoke = commandP "poke" "<type> <segment> <address> <value>" $ do
  typ <- typeP
  (segmentname, segment) <- choice
    [ symbolP "sram" >> return ("sram", retroMemorySaveRam)
    , symbolP "rtc" >> return ("rtc", retroMemoryRtc)
    , symbolP "main" >> return ("main", retroMemorySystemRam)
    , symbolP "video" >> return ("video", retroMemoryVideoRam)
    , symbolP "rom" >> return ("rom", retroMemoryRom)
    ]
  address <- fromIntegral <$> addressP
  value <- fromIntegral <$> integerP
  return $ withLoadedCore [CoreRunning, CorePaused] $ \core -> do
    mdata <- liftIO $ runRetroM core (retroGetMemoryData segment)
    liftIO $ pokeMemoryInteger snesArch typ mdata address value

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
