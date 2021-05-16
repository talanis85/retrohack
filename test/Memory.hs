{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Memory ( tests ) where

import Control.Lens hiding (elements)
import Data.Int
import Data.Word
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

import Retrohack.Memory

instance Arbitrary Signedness where
  arbitrary = elements [ Unsigned, SignedSignMag, SignedOnes, SignedTwos ]

instance Arbitrary Endianness where
  arbitrary = elements [ BigEndian, LittleEndian ]

propIntegerValueRoundtrip :: Int -> Signedness -> Integer -> Endianness -> Property
propIntegerValueRoundtrip size signedness x endianness =
  let val = nullValue signedness endianness size & integerValue .~ fromIntegral x
  in val ^. integerValue === fromIntegral x

tests :: TestTree
tests = testGroup "Memory"
  [ testGroup "big endian examples"
    [ testGroup "unsigned"
      [ testProperty "0x1234" $
          Value Unsigned 2 (Bytes BigEndian [0x12, 0x34]) ^. integerValue === 0x1234
      , testProperty "0x123456" $
          Value Unsigned 3 (Bytes BigEndian [0x12, 0x34, 0x56]) ^. integerValue === 0x123456
      , testProperty "0x12345678" $
          Value Unsigned 4 (Bytes BigEndian [0x12, 0x34, 0x56, 0x78]) ^. integerValue === 0x12345678
      ]
    , testGroup "signed sign-mag" $
      [ testProperty "0" $
          Value SignedSignMag 1 (Bytes BigEndian [0b00000000]) ^. integerValue === 0
      , testProperty "-0" $
          Value SignedSignMag 1 (Bytes BigEndian [0b10000000]) ^. integerValue === 0
      , testProperty "-1" $
          Value SignedSignMag 1 (Bytes BigEndian [0b10000001]) ^. integerValue === (-1)
      , testProperty "-127" $
          Value SignedSignMag 1 (Bytes BigEndian [0b11111111]) ^. integerValue === (-127)
      ]
    ]
  , testGroup "integerValue roundtrip"
      [ testProperty "8-bit unsigned" $ forAll (elements [0..255]) $ \x ->
          propIntegerValueRoundtrip 1 Unsigned x
      , testProperty "8-bit signed (sign-mag)" $ forAll (elements [-127..127]) $ \x ->
          propIntegerValueRoundtrip 1 SignedSignMag x
      , testProperty "8-bit signed (ones complement)" $ forAll (elements [-127..127]) $ \x ->
          propIntegerValueRoundtrip 1 SignedOnes x
      , testProperty "8-bit signed (twos complement)" $ forAll (elements [-128..127]) $ \x ->
          propIntegerValueRoundtrip 1 SignedTwos x
      , testProperty "16-bit unsigned" $ forAll (elements [0..65536]) $ \x ->
          propIntegerValueRoundtrip 2 Unsigned x
      , testProperty "16-bit signed (sign-mag)" $ forAll (elements [-32767..32767]) $ \x ->
          propIntegerValueRoundtrip 2 SignedSignMag x
      , testProperty "16-bit signed (ones complement)" $ forAll (elements [-32767..32767]) $ \x ->
          propIntegerValueRoundtrip 2 SignedOnes x
      , testProperty "16-bit signed (twos complement)" $ forAll (elements [-32768..32767]) $ \x ->
          propIntegerValueRoundtrip 2 SignedTwos x
      ]
  , testGroup "little endian examples"
      [ testProperty "0x1234" $
          Value Unsigned 2 (Bytes LittleEndian [0x34, 0x12]) ^. integerValue === 0x1234
      , testProperty "0x123456" $
          Value Unsigned 3 (Bytes LittleEndian [0x56, 0x34, 0x12]) ^. integerValue === 0x123456
      , testProperty "0x12345678" $
          Value Unsigned 4 (Bytes LittleEndian [0x78, 0x56, 0x34, 0x12]) ^. integerValue === 0x12345678
      ]
  ]
