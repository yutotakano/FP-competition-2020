module Fractals where

import Data.Complex
import Linear

import VConversions
import Color

maxX :: Int
maxX = 1600

maxY :: Int
maxY = 800

setA :: Complex Float
setA = (-0.43) :+ (0.3)

setB :: Complex Float
setB = (0.2) :+ (0.4)

setC :: Complex Float
setC = (-0.7) :+ (0.1)

setD :: Complex Float
setD = (-0.2) :+ (-0.2)

setA2 :: Complex Float
setA2 = (-0.93) :+ (0.3)

setB2 :: Complex Float
setB2 = (0.2) :+ (-0.1)

setC2 :: Complex Float
setC2 = (-0.6) :+ (0.1)

setD2 :: Complex Float
setD2 = (-0.2) :+ (0.7)

-- big blob size depends on this
escapeRadius :: Float
escapeRadius = 5



maxIterations :: Int
maxIterations = 100

maxMagnitude :: Float
maxMagnitude = 10 ** 10

inJulia :: Int -> Int -> Float -> (Complex Float, Complex Float, Complex Float, Complex Float) -> Complex Float -> PixelColor
inJulia n mi r (a, b, c, d) z | magnitude z > maxMagnitude = cBlack
                              | n >= mi && magnitude z < r = cBlack
                              | magnitude z >= r           = cMap n mi
                              | otherwise                  = inJulia (n + 1) mi r (a, b, c, d) updateZ
                -- Moebius transform on z^^2+c
    where
        updateZ = (a * func + b) / (c * func + d)
        func    = z ^^ 3 + z
    -- where updateZ = (a * z ^^ 2 + b) / (c * z ^^ 2 + d)

cMap :: Int -> Int -> PixelColor
cMap n mi = v4IntToWord (V4 (t `mod` (255 ^ 3)) (t `mod` 255 ^ 2) (t `mod` 255) 255)
    where
        t = round ((fI n / fI mi) * fI 32 ^ 3) :: Int
        fI = fromIntegral

colorSet :: [PixelColor]
colorSet = concat [ concat [ [ V4 j k l 255 | l <- [0,8..255] ] | k <- [0,8..255] ] | j <- [0,8..255] ]
