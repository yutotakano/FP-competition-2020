module Main where

import Graphics.Gloss
import System.Directory
import Codec.Picture

import Tracer
import Fractals
import Mandelbrot

window :: Display
-- window = InWindow "RayTracer" (800, 600) (10, 10)
window = InWindow "Pretty Marbles" (xW, yW) (10, 10)

background :: Color
background = white

ourPicture :: Picture
-- ourPicture = bitmapOfByteString maxX maxY (BitmapFormat BottomToTop PxRGBA) juliaImg True
ourPicture = bitmapOfByteString xW yW (BitmapFormat BottomToTop PxRGBA) bitmapData True
-- ourPicture = bitmapOfByteString 800 600 (BitmapFormat BottomToTop PxRGBA) mandImg True

-- ourPictures :: Int -> [Picture]
-- ourPictures is = [ colorsToBitMapData $ antiAliasColors i | i <- [0..is - 1] ]

main :: IO ()
main = display window background ourPicture
