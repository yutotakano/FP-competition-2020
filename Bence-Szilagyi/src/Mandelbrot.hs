module Mandelbrot where

import Data.Complex
import Graphics.Gloss
import System.Directory
import Data.Fixed

import MandelbrotColors
import Color
import Linear

thick = 0.0092906805859* 0.00005
dens = 24.9
disp = 432
m = 850
xdim = 800
ydim = 600

iterations :: Int
iterations = 850

bailout :: Double
bailout = 1.5



getPixel :: (Double, Double) -> PixelColor
getPixel p = mandelbrotColor $ mandelbrot z zd p' iterations
  where
    p' = transformPoint p
    z  = (0.009 / 2) ** (1 / 3)
    zd = 2 * z - 0.009 / (z ** 2)

mandelbrot :: Complex Double -> Complex Double -> Complex Double -> Int -> (Int, Complex Double, Complex Double)
mandelbrot z zd c n
  | magnitude z < bailout && n > 0 = mandelbrot z' zd' c (pred n)
  | otherwise = (n, z, zd)
  where
    z'  = (z * z) + (0.009 / z) + c
    zd' = 2 * z - 0.009 / (z ** 2)

mandelbrotColor :: (Int, Complex Double, Complex Double) -> PixelColor
mandelbrotColor (n, z, z') | n == m || log (magnitude z) * sqrt (magnitude z) < thick * sqrt (magnitude z') = cBlack 
                           | otherwise = V4 (pixelColorChannel "a" k) (pixelColorChannel "b" k) (pixelColorChannel "c" k) 255
  where
    s = fromIntegral n - v * (log (log (magnitude z)) - u)
    k = (dens * s + disp) `mod'` 720

transformPoint :: (Double, Double) -> Complex Double
transformPoint (x, y) = (0.0092906805859 / 800 * x - 0.7028233689263) :+ (0.0092906805859 / 800 * y + 0.1142331238418)


mand :: Int -> Int -> [PixelColor]
mand mx my = concat [ [ getPixel (fI i, fI j) | i <- [1..mx] ] | j <- [1..my] ]
    where fI = fromIntegral

mandImg :: BitMapData
mandImg = colorsToBitMapData $ mand xdim ydim
