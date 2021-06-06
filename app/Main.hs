module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Lens hiding (argument)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Word
import qualified Foreign.Lua as Lua
import Options.Applicative
import System.Console.Haskeline hiding (finally)
import System.Environment
import System.FilePath
import Text.Printf

import AppM
import SyncTVar
import Command
import Retrohack.Memory
import Libretro
import Lua
import Audio
import Video

data Options = Options
  { optCore :: String
  , optGame :: String
  , optScripts :: [ScriptOptions]
  }

data ScriptOptions = ScriptOptions
  { scriptFile :: FilePath
  , scriptArguments :: [String]
  }

options :: ParserInfo Options
options = info (helper <*> (Options <$> optCoreP <*> optGameP <*> many optScriptP))
  (  fullDesc
  <> progDesc "Libretro frontend with lua support"
  <> header "retrohack"
  <> footer ("Version: 0.0.1")
  )

optCoreP :: Parser FilePath
optCoreP = argument str (metavar "CORE" <> help "Path to the libretro core")

optGameP :: Parser FilePath
optGameP = argument str (metavar "GAME" <> help "Path to the game rom")

optScriptP :: Parser ScriptOptions
optScriptP = subparser (command "script" (info (ScriptOptions <$> argScriptNameP <*> many argScriptArgP) (progDesc "Load a script")))

argScriptNameP :: Parser FilePath
argScriptNameP = argument str (metavar "SCRIPTFILE" <> help "Path to the lua script")

argScriptArgP :: Parser String
argScriptArgP = argument str (metavar "COMMAND" <> help "Lua statement to be executed at initialization")

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
  opts <- execParser options

  audio <- initAudio
  video <- initVideo
  state <- initAppState audio video

  let initActions = do
        runCommand $ "loadcore \"" ++ optCore opts ++ "\""
        runCommand $ "loadgame \"" ++ optGame opts ++ "\""
        forM_ (optScripts opts) $ \x -> do
          runCommand $ "luaload \"" ++ scriptFile x ++ "\""
          forM_ (scriptArguments x) $ \arg -> do
            runCommand $ "luaexec \"" ++ scriptFile x ++ "\" \"" ++ arg ++ "\""

  evalAppM (runInputT defaultSettings (inputLoop initActions)) state

inputLoop :: AppM () -> InputT AppM ()
inputLoop initActions = do
  printFun <- getExternalPrint
  lift $ appPrint .= printFun
  lift $ initActions
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

tryLua :: IO a -> IO (Either Lua.Exception a)
tryLua = try

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
  , cmdLuaLoad
  , cmdLuaUnload
  , cmdLuaStatus
  , cmdLuaExec
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
    printFun <- use appPrint

    taskQueue <- use appMainTasks
    runningVar <- use appCoreRunning
    luaThreadsVar <- use appLuaThreads

    liftIO $ forkIO $ do
      audioResult <- tryIO $
        configureAudio audio (floor (retroSystemTimingSampleRate (retroSystemAvInfoTiming avInfo)))
      case audioResult of
        Left err -> atomically $ writeTQueue taskQueue (output $ printf "IOException: %s" (show err))
        Right () -> return ()

      configureVideo video (retroSystemAvInfoGeometry avInfo)

      emulatorProcess core video runningVar taskQueue luaThreadsVar printFun

      deconfigureVideo video
      runRetroM core $ retroDeinit
      runRetroM core $ retroInit

    appCore . traversed . _2 .= CoreRunning

    output "Game is running"

emulatorProcess
  :: RetroCore
  -> Video
  -> TMVar ()
  -> TQueue (AppM ())
  -> SyncTVar (Map.Map String LuaThread)
  -> (String -> IO ())
  -> IO ()
emulatorProcess core video runningVar taskQueue luaThreadsVar printFun = do
  runLuaHook "retrohack_start"
  loop
  runLuaHook "retrohack_stop"
  where
    runLuaHook name = do
      luaThreads <- atomically $ readAndLock luaThreadsVar
      luaThreads' <- tryLua $ forM luaThreads $ \thread ->
        runWithLuaThread thread (Lua.callFunc name :: Lua.Lua ())
      case luaThreads' of
        Left err -> do
          atomically $ do
            unlockSyncTVar luaThreadsVar
            tryTakeTMVar runningVar
            writeTQueue taskQueue $ do
              appCore . traversed . _2 .= CorePaused
          printFun (show err)
        Right luaThreads'' -> atomically $ writeAndUnlock luaThreadsVar luaThreads''

    loop = do
      atomically (takeTMVar runningVar >> putTMVar runningVar ())

      runLuaHook "retrohack_frame"

      runRetroM core retroRun
      videoRender video

      shouldClose <- windowShouldClose video
      if shouldClose
        then atomically $ writeTQueue taskQueue $ do
          appCore . traversed . _2 .= CoreStopped
        else loop

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

cmdLuaLoad :: Command
cmdLuaLoad = commandP "luaload" "<filename>" $ do
  filename <- stringP
  return $ withLoadedCore [CoreStopped, CorePaused, CoreRunning] $ \core -> do
    printFun <- use appPrint

    newThread <- liftIO $ tryLua $ do
      luaThread <- liftIO $ createLuaThread
      liftIO $ runWithLuaThread luaThread $ do
        Lua.openlibs
        Lua.dostring (BS8.pack (printf "package.path = package.path .. ';%s/?.lua'" (takeDirectory filename)))
        Lua.registerHaskellFunction "output" (luafOutput printFun)
        Lua.registerHaskellFunction "memory_read" (luafMemoryRead core)
        Lua.registerHaskellFunction "memory_write" (luafMemoryWrite core)
        status <- Lua.dofile filename
        when (status /= Lua.OK) $ Lua.throwTopMessage

    case newThread of
      Left err -> output (show err)
      Right newThread' -> withLuaThreads $ do
        threads <- get
        liftIO $ mapM_ destroyLuaThread $ Map.lookup filename threads
        modify $ Map.insert filename newThread'

cmdLuaUnload :: Command
cmdLuaUnload = commandP "luaunload" "<filename>" $ do
  filename <- stringP
  return $ withLuaThreads $ do
    threads <- get
    case Map.lookup filename threads of
      Nothing -> do
        lift $ output $ printf "Script %s is not loaded" filename
      Just oldThread' -> do
        liftIO $ destroyLuaThread oldThread'
        modify $ Map.delete filename

cmdLuaStatus :: Command
cmdLuaStatus = commandP "luastatus" "" $ do
  return $ do
    threadsVar <- use appLuaThreads
    threads <- liftIO $ atomically $ readSyncTVar threadsVar
    forM_ (Map.toAscList threads) $ \(filename, _) -> do
      output filename

cmdLuaExec :: Command
cmdLuaExec = commandP "luaexec" "<filename> <command>" $ do
  filename <- stringP
  cmd <- stringP
  return $ withLuaThreads $ do
    threads <- get
    case Map.lookup filename threads of
      Nothing -> lift $ output $ printf "No thread called '%s'" filename
      Just thread -> do
        thread' <- liftIO $ runWithLuaThread thread $ Lua.dostring (BS8.pack (cmd ++ "\n"))
        modify $ Map.insert filename thread'

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
