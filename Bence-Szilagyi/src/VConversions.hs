{-

Welcome to Nightmare Town
Hope you enjoy your stay

-}

module VConversions where

import Linear
import Linear.V
import Data.Foldable
import Data.Vector
import Data.Word

listToV2 :: [a] -> V2 a
listToV2 xs = (fromV :: V (Size V2) a -> V2 a) (V (Data.Vector.fromList xs))

listToV3 :: [a] -> V3 a
listToV3 xs = (fromV :: V (Size V3) a -> V3 a) (V (Data.Vector.fromList xs))

listToV4 :: [a] -> V4 a
listToV4 xs = (fromV :: V (Size V4) a -> V4 a) (V (Data.Vector.fromList xs))

wordToFloat :: Word8 -> Float
wordToFloat = fromIntegral

v4WordToFloat :: V4 Word8 -> V4 Float
v4WordToFloat = fmap wordToFloat

wordToInt :: Word8 -> Int
wordToInt = fromIntegral

v4WordToInt :: V4 Word8 -> V4 Int
v4WordToInt = fmap wordToInt

intToWord :: Int -> Word8
intToWord = fromIntegral

v4IntToWord :: V4 Int -> V4 Word8
v4IntToWord = fmap intToWord

v4Round :: V4 Float -> V4 Int
v4Round = fmap round

sumV2Float :: V2 Float -> Float
sumV2Float = Data.Foldable.sum

sumV3Float :: V3 Float -> Float
sumV3Float = Data.Foldable.sum

v4FTW :: V4 Float -> V4 Word8
v4FTW = v4IntToWord . v4Round
