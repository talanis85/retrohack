{-# LANGUAGE TemplateHaskell #-}
module AppM
  ( AppM
  , evalAppM
  , CoreState (..)
  , AppState (..)
  , initAppState
  , appCore
  , appVideo
  , appPrint
  , appMainTasks
  , runAppTasks
  , output
  ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.State.Strict

import Libretro
import Video

type AppM = StateT AppState IO

evalAppM :: AppM a -> AppState -> IO a
evalAppM = evalStateT

data CoreState = CoreFresh | CoreInitialized | CoreRunning
  deriving (Eq, Show)

data AppState = AppState
  { _appCore :: Maybe (RetroCore, CoreState)
  , _appVideo :: Video
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

initAppState :: Video -> IO AppState
initAppState video = do
  taskQueue <- atomically newTQueue

  return AppState
    { _appCore = Nothing
    , _appVideo = video
    , _appPrint = liftIO . putStrLn
    , _appMainTasks = taskQueue
    }
