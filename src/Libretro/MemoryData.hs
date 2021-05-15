module Libretro.MemoryData
  ( MemoryData
  , memoryData
  , memoryDataSize
  , peekMemory
  , peekMemoryI8
  , peekMemoryI16
  , peekMemoryI32
  , peekMemoryU8
  , peekMemoryU16
  , peekMemoryU32
  , peekMemoryMono
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

peekMemory :: (Storable a) => MemoryData -> Int -> IO (Maybe a)
peekMemory (MemoryData size ptr) i
  | i < 0 || i >= size = return Nothing
  | otherwise = Just <$> peekByteOff ptr i

pokeMemory :: (Storable a) => MemoryData -> Int -> a -> IO ()
pokeMemory (MemoryData size ptr) i x
  | i < 0 || i >= size = return ()
  | otherwise = pokeByteOff ptr i x

peekMemoryI8 :: MemoryData -> Int -> IO (Maybe Int8)
peekMemoryI8 = peekMemory

peekMemoryI16 :: MemoryData -> Int -> IO (Maybe Int16)
peekMemoryI16 = peekMemory

peekMemoryI32 :: MemoryData -> Int -> IO (Maybe Int32)
peekMemoryI32 = peekMemory

peekMemoryU8 :: MemoryData -> Int -> IO (Maybe Word8)
peekMemoryU8 = peekMemory

peekMemoryU16 :: MemoryData -> Int -> IO (Maybe Word16)
peekMemoryU16 = peekMemory

peekMemoryU32 :: MemoryData -> Int -> IO (Maybe Word32)
peekMemoryU32 = peekMemory

peekMemoryMono :: (Integral a) => (MemoryData -> Int -> IO (Maybe a)) -> MemoryData -> Int -> IO (Maybe Integer)
peekMemoryMono f mdata i = fmap fromIntegral <$> f mdata i
