module SyncTVar
  ( SyncTVar
  , newSyncTVar
  , readSyncTVar
  , unlockSyncTVar
  , readAndLock
  , writeAndUnlock
  ) where

import Control.Concurrent.STM

data SyncTVar a = SyncTVar (TMVar ()) (TVar a)

newSyncTVar :: a -> STM (SyncTVar a)
newSyncTVar x = do
  mutex <- newTMVar ()
  var <- newTVar x
  return (SyncTVar mutex var)

readSyncTVar :: SyncTVar a -> STM a
readSyncTVar (SyncTVar _ var) = readTVar var

readAndLock :: SyncTVar a -> STM a
readAndLock (SyncTVar mutex var) = takeTMVar mutex >> readTVar var

writeAndUnlock :: SyncTVar a -> a -> STM ()
writeAndUnlock (SyncTVar mutex var) x = writeTVar var x >> putTMVar mutex ()

unlockSyncTVar :: SyncTVar a -> STM ()
unlockSyncTVar (SyncTVar mutex _) = putTMVar mutex ()
