module Retrohack.Memory
  ( Endianness (..)
  , Signedness (..)
  , Bytes (..)
  , Value (..)
  , DataType (..)

  , MemoryArch
  , snesArch

  , integerValue
  , formatValue
  , peekMemoryValue
  , pokeMemoryInteger
  , nullValue
  ) where

import Control.Lens
import Data.Bits
import Data.List
import Data.Word
import Text.Printf

import Libretro.MemoryData

data Endianness = BigEndian | LittleEndian
  deriving (Eq, Show)
data Signedness = Unsigned | SignedSignMag | SignedOnes | SignedTwos
  deriving (Eq, Show)

data DataType = U8 | U16 | U32 | I8 | I16 | I32
  deriving (Eq, Show)

type MemoryArch = DataType -> (Int, Endianness, Signedness)

snesArch :: MemoryArch
snesArch U8 = (1, LittleEndian, Unsigned)
snesArch U16 = (2, LittleEndian, Unsigned)
snesArch U32 = (4, LittleEndian, Unsigned)
snesArch I8 = (1, LittleEndian, SignedTwos)
snesArch I16 = (2, LittleEndian, SignedTwos)
snesArch I32 = (4, LittleEndian, SignedTwos)

data Bytes = Bytes Endianness [Word8]
  deriving (Eq, Show)
data Value = Value Signedness Int Bytes
  deriving (Eq, Show)

peekMemoryValue :: MemoryArch -> DataType -> MemoryData -> Int -> IO (Maybe Value)
peekMemoryValue arch typ mdata i = do
  let (size, endianness, signedness) = arch typ

  bytes <- sequence <$> mapM (\i' -> peekMemory mdata i') [i..(i+size-1)]
  return (Value signedness size <$> Bytes endianness <$> bytes)

pokeMemoryInteger :: MemoryArch -> DataType -> MemoryData -> Int -> Integer -> IO ()
pokeMemoryInteger arch typ mdata i val = do
  let (size, endianness, signedness) = arch typ
  let (Value _ _ (Bytes _ bytes)) = nullValue signedness endianness size & integerValue .~ val
  mapM_ (\(i', x) -> pokeMemory mdata i' x) (zip [i..] bytes)

signednessLens :: Signedness -> Lens' [Word8] Integer
signednessLens Unsigned = unsigned
signednessLens SignedSignMag = signMag
signednessLens SignedOnes = onesComplement
signednessLens SignedTwos = twosComplement

nullValue :: Signedness -> Endianness -> Int -> Value
nullValue s e size = Value s size (Bytes e [])

integerValue :: Lens' Value Integer
integerValue = lens getter setter
  where
    getter (Value signed size bytes) = bytes ^. bigEndian . sized size . signednessLens signed
    setter (Value signed size bytes) x = Value signed size (bytes & bigEndian . sized size . signednessLens signed .~ x)

formatValue :: Getter Value String
formatValue = to formatValue'
  where
    formatValue' x@(Value signed size bytes) = case signed of
      Unsigned -> printf ("0x%0" ++ show size ++ "x (%d)") (x ^. integerValue) (x ^. integerValue)
      _ -> printf "%d" (x ^. integerValue)

sized :: (Num a) => Int -> Lens' [a] [a]
sized size = lens getter setter
  where
    getter = trimToSize size
    setter _ = trimToSize size

trimToSize :: (Num a) => Int -> [a] -> [a]
trimToSize s bytes =
  let trimmed = drop (max 0 (length bytes - s))  bytes
  in replicate (s - length trimmed) 0 ++ trimmed

littleEndian :: Lens' Bytes [Word8]
littleEndian = lens getter setter
  where
    getter (Bytes LittleEndian xs) = xs
    getter (Bytes BigEndian xs) = reverse xs
    setter (Bytes LittleEndian _) xs = Bytes LittleEndian xs
    setter (Bytes BigEndian _) xs = Bytes BigEndian (reverse xs)

bigEndian :: Lens' Bytes [Word8]
bigEndian = lens getter setter
  where
    getter (Bytes LittleEndian xs) = reverse xs
    getter (Bytes BigEndian xs) = xs
    setter (Bytes LittleEndian _) xs = Bytes LittleEndian (reverse xs)
    setter (Bytes BigEndian _) xs = Bytes BigEndian xs

unsigned :: Iso' [Word8] Integer
unsigned = iso getter setter
  where
    getter :: [Word8] -> Integer
    getter = foldl foldBytes 0
    setter :: Integer -> [Word8]
    setter x = reverse $ unfoldr unfoldBytes x

signMag :: Lens' [Word8] Integer
signMag = lens getter setter
  where
    splitSign [] = (False, [])
    splitSign (x:xs) = (x `testBit` 7, (x `clearBit` 7) : xs)
    addSign s [] = []
    addSign False xs = xs
    addSign True (x:xs) = (x `setBit` 7) : xs
    getter xs =
      let (s, xs') = splitSign xs
          absval = foldl foldBytes 0 xs'
      in if s then negate absval else absval
    setter original x = addSign (x < 0) (trimToSize (length original) $ reverse $ unfoldr unfoldBytes (abs x))

onesComplement :: Lens' [Word8] Integer
onesComplement = lens getter setter
  where
    splitSign [] = (False, [])
    splitSign (x:xs) =
      let sign = x `testBit` 7
      in if sign
            then (sign, map complement (x : xs))
            else (sign, (x `clearBit` 7) : xs)
    addSign s [] = []
    addSign False xs = xs
    addSign True (x:xs) = (complement x `setBit` 7) : map complement xs
    getter xs =
      let (s, xs') = splitSign xs
          absval = foldl foldBytes 0 xs'
      in if s then negate absval else absval
    setter original x = addSign (x < 0) (trimToSize (length original) $ reverse $ unfoldr unfoldBytes (abs x))

twosComplement :: Lens' [Word8] Integer
twosComplement = lens getter setter
  where
    splitSign [] = (False, [])
    splitSign (x:xs) =
      let sign = x `testBit` 7
      in if sign
            then (sign, map complement (decrementBytes (x : xs)))
            else (sign, (x `clearBit` 7) : xs)
    addSign s [] = []
    addSign False xs = xs
    addSign True (x:xs) = incrementBytes ((complement x `setBit` 7) : map complement xs)
    getter xs =
      let (s, xs') = splitSign xs
          absval = foldl foldBytes 0 xs'
      in if s then negate absval else absval
    setter original x = addSign (x < 0) (trimToSize (length original) $ reverse $ unfoldr unfoldBytes (abs x))

foldBytes :: (Num a) => a -> Word8 -> a
foldBytes a x = a * 0x100 + fromIntegral x

unfoldBytes :: (Integral a) => a -> Maybe (Word8, a)
unfoldBytes x
  | x < 0 = error "unfoldBytes with x < 0"
  | x == 0 = Nothing
  | otherwise = Just (fromIntegral (x `mod` 0x100), x `div` 0x100)

incrementBytes :: [Word8] -> [Word8]
incrementBytes = reverse . incrementBytes' . reverse
  where
    incrementBytes' [] = []
    incrementBytes' (x:xs) =
      let x' = x + 1
      in if x' == 0 then x' : incrementBytes' xs else x' : xs

decrementBytes :: [Word8] -> [Word8]
decrementBytes = reverse . decrementBytes' . reverse
  where
    decrementBytes' [] = []
    decrementBytes' (x:xs) =
      let x' = x - 1
      in if x' == 0xff then x' : decrementBytes' xs else x' : xs
