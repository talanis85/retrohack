{-# LANGUAGE TemplateHaskell #-}
module AppM
  ( AppM
  , evalAppM
  , CoreState (..)
  , formatCoreState
  , AppState (..)
  , initAppState
  , appCore
  , appVideo
  , appAudio
  , appPrint
  , appMainTasks
  , runAppTasks
  , output
  ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.State.Strict

import Libretro
import Audio
import Video

type AppM = StateT AppState IO

evalAppM :: AppM a -> AppState -> IO a
evalAppM = evalStateT

data CoreState = CoreFresh | CoreStopped | CoreRunning
  deriving (Eq, Show)

formatCoreState :: CoreState -> String
formatCoreState CoreFresh = "loaded"
formatCoreState CoreStopped = "stopped"
formatCoreState CoreRunning = "running"

data AppState = AppState
  { _appCore :: Maybe (RetroCore, CoreState)
  , _appVideo :: Video
  , _appAudio :: Audio
  , _appPrint :: String -> AppM ()
  , _appMainTasks :: TQueue (AppM ())
  }

makeLenses ''AppState

output :: String -> AppM ()
output s = do
  f <- use appPrint
  f s

runAppTasks :: AppM ()
runAppTasks = do
  taskQueue <- use appMainTasks
  tasks <- liftIO $ atomically $ flushTQueue taskQueue
  sequence_ tasks

initAppState :: Audio -> Video -> IO AppState
initAppState audio video = do
  taskQueue <- atomically newTQueue

  return AppState
    { _appCore = Nothing
    , _appVideo = video
    , _appAudio = audio
    , _appPrint = liftIO . putStrLn
    , _appMainTasks = taskQueue
    }
