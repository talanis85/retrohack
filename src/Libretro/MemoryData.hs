module Libretro.MemoryData
  ( MemoryData
  , memoryData
  , memoryDataSize
  , peekMemory
  , pokeMemory
  ) where

import Data.Int
import Data.Word
import Foreign

data MemoryData = MemoryData Int (Ptr ())

memoryData :: Int -> Ptr a -> MemoryData
memoryData size ptr = MemoryData size (castPtr ptr)

memoryDataSize :: MemoryData -> Int
memoryDataSize (MemoryData size _) = size

peekMemory :: MemoryData -> Int -> IO (Maybe Word8)
peekMemory (MemoryData size ptr) i
  | i < 0 || i >= size = return Nothing
  | otherwise = Just <$> peekByteOff ptr i

pokeMemory :: MemoryData -> Int -> Word8 -> IO ()
pokeMemory (MemoryData size ptr) i x
  | i < 0 || i >= size = return ()
  | otherwise = pokeByteOff ptr i x
