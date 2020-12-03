module Tracer where

import Linear
import Data.Word
import Data.ByteString (ByteString)
import Data.Foldable
import System.Random
import Data.Complex
import Data.Ord
import Control.Parallel.Strategies

import VConversions
import VOperations
import Color
import Ray
import SphereHitCheck
import Materials
import Fractals
import Stereographic
import Mandelbrot

-- Window dimensions
xW :: Int
xW = 1600

xWF :: Float
xWF = fromIntegral xW

yW :: Int
yW = 800

yWF :: Float
yWF = fromIntegral yW

aAMult :: Int
aAMult = 100

-- Actual image plane definition starts here
ratio :: Float
ratio = 100

-- How far "into the screen" our image plane is (times -1)
zW :: Float
zW = 2

bitmapData :: ByteString
bitmapData = colorsToBitMapData $ antiAliasColors 0
-- bitmapData = colorsToBitMapData colors



-- fovControl :: Float
-- fovControl = 1 / 4

lowerLeft :: Point3D
-- lowerLeft = (/ ratio) <$> listToV3 [ -xWF * fovControl, -yWF * fovControl, -zW ]
lowerLeft = V3 (-2) (-1) (-2)

imageLength :: V3 Float
-- imageLength = listToV3 [ xWF * fovControl / ratio, 0.0, 0.0 ]
imageLength = V3 4 0 0

imageHeight :: V3 Float
-- imageHeight = listToV3 [ 0.0, yWF * fovControl / ratio, 0.0 ]
imageHeight = V3 0 2 0

scatterer :: StdGen
scatterer = mkStdGen 360

rayColor :: Int -> Int -> Ray -> Sphere -> Point3D -> Int -> Int -> V4 Float
rayColor frame randSeed ray lastSph lastHP x y | p > 0.001           = reflectivity (material sph) * rayColor frame (randSeed + 1) (Ray hitPoint (bounce m n (direction ray) randSeed)) sph hitPoint 0 0
                                               | lastSph /= NoSphere = v4WordToFloat $ inJulia 1 maxIterations escapeRadius (fractalInit lastSph) (head l :+ last l)
                                        --  | lastSph /= NoSphere = v4WordToFloat $ getPixelColor (800 * realToFrac xdir) (600 * realToFrac ydir)
                                        --  | otherwise           = v4WordToFloat $ lerpColors (intToWord $ round (0.5 * (toList (direction ray) !! 1 + 1) * 255)) cBlue cWhite
                                               | otherwise           = v4WordToFloat $ getPixel (fromIntegral x / fromIntegral xW * 800, fromIntegral y / fromIntegral yW * 600)
    where
        -- list comprehension over list of hittable objects here to get the param
        listPNSs                   = filter (\(p, _, _) -> p > 0) 
                                        [ let p = hitSphere (if sph /= NoSphere 
                                                                then Sph (radius sph) (center sph + (fromIntegral frame *^ V3 1.0 1.0 (-1.0))) (material sph) (fractalInit sph) 
                                                                else NoSphere) ray in 
                                            if p > 0 
                                                then (p, mkUnitV3 (pAtParam p ray - center sph), sph) 
                                                else (p, V3 0 0 0, NoSphere) | sph <- sphereList ]
        pnmf | not $ null listPNSs = minimumBy (comparing (\(p, _, _) -> p)) listPNSs
             | otherwise            = (-1.0, V3 0 0 0, NoSphere)
        p                           = (\(p, _, _)   -> p)   pnmf
        n                           = (\(_, n, _)   -> n)   pnmf
        sph                         = (\(_, _, sph) -> sph) pnmf
        m | sph /= NoSphere         = material sph
          | otherwise               = NoMat
        hitPoint                    = pAtParam p ray
        l                           = toList $ sgSP lastSph lastHP
        -- xydir                       = toList $ mkUnitV2 $ listToV2 $ take 2 (toList $ direction ray)
        -- xdir                        = head xydir
        -- ydir                        = last xydir

colors :: Int -> [PixelColor]
colors i = concat [ [ v4FTW $ rayColor i (head (randoms (sphR $ x * y) :: [Int])) (Ray originPoint (dir x y 0.5 0.5)) NoSphere originPoint x y | x <- [0..xW-1] ] | y <- [0..yW-1] ]

dir :: Int -> Int -> Float -> Float -> V3 Float
dir x y rx ry = mkUnitV3 (lowerLeft + (imageLength ^* ((fromIntegral x + rx) / xWF)) + (imageHeight ^* ((fromIntegral y + ry) / yWF)) - originPoint)



-------------------
-- Anti-aliasing --
-------------------

-- This is needed for diffuse materials to look right, as the other version only has 1 ray per pixel
antiAliasColors :: Int -> [PixelColor]
antiAliasColors j = concat ([ [ v4FTW $ avgColors j x y $ randomDirs x y aAMult | x <- [0..xW-1] ] | y <- [0..yW-1] ] `using` parList rdeepseq)

-- The random pattern will be the same for x and y, but that shouldn't really matter
randomDirs :: Int -> Int -> Int -> [V3 Float]
randomDirs x y n = [ dir x y (xRand!!i) (yRand!!i) | i <- [1..n]]

avgColors :: Int -> Int -> Int -> [V3 Float] -> V4 Float
avgColors j x y dirs = sum cs ^* (1 / fromIntegral (length dirs))
    where
        cs = [ rayColor j (head (randoms (sphR $ x + y * xW + i * xW * yW) :: [Int])) (Ray originPoint d) NoSphere originPoint x y :: V4 Float | (d,i) <- zip dirs [1..length dirs] ]

-- Randomness for anti-aliasing
ran :: StdGen
ran = mkStdGen 42
xR :: StdGen
yR :: StdGen
(xR, yR) = split ran
xRand :: [Float]
xRand = randomRs (0.0, 1.0) xR :: [Float]
yRand :: [Float]
yRand = randomRs (0.0, 1.0) yR :: [Float]
