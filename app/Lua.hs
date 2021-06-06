{-# LANGUAGE TemplateHaskell #-}
module Lua
  ( LuaThread (..)
  , luaThreadState
  , createLuaThread
  , destroyLuaThread
  , runWithLuaThread

  , luafOutput
  , luafMemoryRead
  , luafMemoryWrite
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Foreign.Lua as Lua

import Libretro
import Retrohack.Memory

data LuaValue = LuaBool Bool | LuaNumber Integer | LuaString T.Text

makePrisms ''LuaValue

data LuaThread = LuaThread
  { _luaThreadState :: Lua.State
  }

makeLenses ''LuaThread

createLuaThread :: IO LuaThread
createLuaThread = do
  state <- Lua.newstate

  return LuaThread
    { _luaThreadState = state
    }

destroyLuaThread :: LuaThread -> IO ()
destroyLuaThread thread = Lua.close (thread ^. luaThreadState)

runWithLuaThread :: LuaThread -> Lua.Lua a -> IO LuaThread
runWithLuaThread thread action = do
  newState <- Lua.runWith (thread ^. luaThreadState) (action >> Lua.state)
  return $ thread & luaThreadState .~ newState

{-
initAction :: LuaEnv -> Lua.Lua ()
initAction envTVar = do
  Lua.openlibs
  Lua.registerHaskellFunction "getenvBool" (luafGetEnvBool envTVar)

luafGetEnvBool :: LuaEnv -> T.Text -> Lua.Lua (Lua.Optional Bool)
luafGetEnvBool envTVar key = Lua.Optional <$> (>>= preview _LuaBool)
  <$> Map.lookup (T.unpack key) <$> liftIO (atomically (readTVar envTVar))
-}

luafOutput :: (String -> IO ()) -> T.Text -> Lua.Lua ()
luafOutput printFun s = liftIO (printFun (T.unpack s))

parseSegment :: String -> Maybe RetroMemory
parseSegment "sram"  = Just retroMemorySaveRam
parseSegment "rtc"   = Just retroMemoryRtc
parseSegment "main"  = Just retroMemorySystemRam
parseSegment "video" = Just retroMemoryVideoRam
parseSegment "rom"   = Just retroMemoryRom
parseSegmnt _ = Nothing

luafMemoryRead :: RetroCore -> String -> String -> Int -> Lua.Lua Integer
luafMemoryRead core segmentstr typstr address = do
  typ <- case parseDataType typstr of
    Nothing -> Lua.throwException $ "Invalid type '" ++ typstr ++ "'"
    Just typ -> return typ

  segment <- case parseSegment segmentstr of
    Nothing -> Lua.throwException $ "Invalid memory segment '" ++ segmentstr ++ "'"
    Just segment -> return segment

  mdata <- liftIO $ runRetroM core (retroGetMemoryData segment)
  value <- liftIO $ peekMemoryValue snesArch typ mdata address

  case value of
    Nothing -> Lua.throwException $ "Invalid memory address " ++ show address
    Just value' -> return $ value' ^. integerValue

luafMemoryWrite :: RetroCore -> String -> String -> Int -> Integer -> Lua.Lua ()
luafMemoryWrite core segmentstr typstr address value = do
  typ <- case parseDataType typstr of
    Nothing -> Lua.throwException $ "Invalid type '" ++ typstr ++ "'"
    Just typ -> return typ

  segment <- case parseSegment segmentstr of
    Nothing -> Lua.throwException $ "Invalid memory segment '" ++ segmentstr ++ "'"
    Just segment -> return segment

  mdata <- liftIO $ runRetroM core (retroGetMemoryData segment)
  liftIO $ pokeMemoryInteger snesArch typ mdata address value
