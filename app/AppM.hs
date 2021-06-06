{-# LANGUAGE TemplateHaskell #-}
module AppM
  ( AppM
  , evalAppM
  , CoreState (..)
  , formatCoreState
  , AppState (..)
  , initAppState
  , appCore
  , appCoreState
  , appGame
  , appVideo
  , appAudio
  , appPrint
  , appMainTasks
  , appCoreRunning
  , appLuaThreads
  , runAppTasks
  , withLuaThreads
  , output
  ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as Map

import SyncTVar
import Libretro
import Lua
import Audio
import Video

type AppM = StateT AppState IO

evalAppM :: AppM a -> AppState -> IO a
evalAppM = evalStateT

data CoreState = CoreFresh | CoreStopped | CoreRunning | CorePaused
  deriving (Eq, Show)

formatCoreState :: CoreState -> String
formatCoreState CoreFresh = "loaded"
formatCoreState CoreStopped = "stopped"
formatCoreState CoreRunning = "running"
formatCoreState CorePaused = "paused"

data AppState = AppState
  { _appCore :: RetroCore
  , _appCoreState :: CoreState
  , _appGame :: FilePath
  , _appVideo :: Video
  , _appAudio :: Audio
  , _appPrint :: String -> IO ()
  , _appMainTasks :: TQueue (AppM ())
  , _appCoreRunning :: TMVar ()
  , _appLuaThreads :: SyncTVar (Map.Map String LuaThread)
  }

makeLenses ''AppState

output :: String -> AppM ()
output s = do
  f <- use appPrint
  liftIO $ f s

runAppTasks :: AppM ()
runAppTasks = do
  taskQueue <- use appMainTasks
  tasks <- liftIO $ atomically $ flushTQueue taskQueue
  sequence_ tasks

initAppState :: RetroCore -> FilePath -> Audio -> Video -> IO AppState
initAppState core game audio video = do
  taskQueue <- atomically newTQueue
  runningVar <- atomically (newTMVar ())
  threads <- atomically (newSyncTVar Map.empty)

  return AppState
    { _appCore = core
    , _appCoreState = CoreFresh
    , _appGame = game
    , _appVideo = video
    , _appAudio = audio
    , _appPrint = putStrLn
    , _appMainTasks = taskQueue
    , _appCoreRunning = runningVar
    , _appLuaThreads = threads
    }

withLuaThreads :: StateT (Map.Map String LuaThread) AppM a -> AppM a
withLuaThreads x = do
  threadsVar <- use appLuaThreads
  threads <- liftIO $ atomically $ readAndLock threadsVar
  (a, threads') <- runStateT x threads
  liftIO $ atomically $ writeAndUnlock threadsVar threads'
  return a
