module Color where

import Linear
import Data.Word
import Data.Foldable
import Data.ByteString (ByteString, pack)

import VConversions

type PixelColor = V4 Word8

type BitMapData = ByteString

cBlue :: PixelColor
cBlue = V4 64 64 232 200

cGreen :: PixelColor
cGreen = V4 64 232 64 200

cRed :: PixelColor
cRed = V4 232 64 64 200

cWhite :: PixelColor
cWhite = V4 255 255 255 200

cBlack :: PixelColor
cBlack = V4 0 0 0 255

wordListToColor :: [Word8] -> PixelColor
wordListToColor = listToV4

colorsToBitMapData :: [PixelColor] -> BitMapData
colorsToBitMapData cs = pack $ concatMap toList cs

lerpColors :: Word8 -> PixelColor -> PixelColor -> PixelColor
lerpColors w c1 c2 = v4IntToWord $ v4Round $ lerp (wordToFloat w /255) (v4WordToFloat c1) (v4WordToFloat c2)
