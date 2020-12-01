module Main where

import Codec.Picture

data Point = Point (Float, Float)

addPoints :: Point -> Point -> Point
addPoints (Point (x1, y1)) (Point (x2, y2)) = (Point ((x1+x2),(y1+y2)))

multPoints :: Float -> Point -> Point
multPoints n (Point (x, y)) = Point ((n * x),(n * y))

absPoint :: Point -> Float
absPoint (Point (x, y)) = (x ^ 2) + (y ^ 2)

-- change these dimensions for larger renderings
width :: Float
width = 600

height :: Float
height = 600

zoomVals :: [(Float,Int)]
zoomVals = zip [6-0.04*i | i <- [0..149]] [0..]

--makes the gif in the classic blue-ish green colour my nightmares are made of
main = sequence_ $ writeGifAnimation "out.gif" 7 LoopingForever [ convertRGB8 $ ImageRGB8 (generateImage (makeCreepyFrac (fst i)) (round width) (round height)) | i <- zoomVals ]

--makes a single still image of the above
--main = savePngImage "out.png" (ImageRGB8 (generateImage (makeCreepyFrac 5) 1800 1800 ) )

--makes the gif in a vapourwave-ish colour scheme, also the gradient is animated
--main = sequence_ $ writeGifAnimation "outvw.gif" 7 LoopingForever
--        [ convertRGB8 $ ImageRGB8 (generateImage (makeVapourwaveCreepyFrac (fst i) (snd i) ) (round width) (round height)) | i <- zoomVals ]

--makes a single still image of the above
--main = savePngImage "outvw.png" (ImageRGB8 (generateImage (makeVapourwaveCreepyFrac 5 50) 1800 1800 ) )

makeOnePic :: Float -> IO ()
makeOnePic i = savePngImage "out.png" (ImageRGB8 (generateImage (makeCreepyFrac i) 1800 1800 ) )

-- this is applied on every component of the point
boxFold :: Float -> Float
boxFold c
    | c > 1 = 2 - c
    | c < (-1) = (-2) - c
    | otherwise = c

-- this is applied on the whole point after boxFold is applied to its components
ballFold :: Point -> Point
ballFold (Point (x, y))
    | sqrt(x^2+y^2) < 0.5 = Point ((x * 4), (y * 4))
    | sqrt(x^2+y^2) < 1 = Point ((x / (x^2+y^2)), (y / (x^2+y^2)))
    | otherwise = Point (x, y)

-- combining the two functions above to make a big function
bigFunc :: Point -> Point
bigFunc (Point (x, y)) = addPoints (Point (x, y)) $ multPoints 2 $ ballFold (Point ((boxFold x) , (boxFold y)))

toCoord :: Float -> Point -> Point
toCoord i (Point (x, y))= Point (((-i) + (2*i) * (x / width)), ((-i) + (2*i) * (y / height)))

-- function that wraps up the above, takes a point (x,y) to its pixel value based on the functions above
creepyFrac :: Point -> Int -> Int -> Int
creepyFrac point@(Point (x, y)) iter maxIter
    | sqrt (absPoint point) < 4.0 && iter < maxIter = creepyFrac (bigFunc point) (iter + 1) maxIter
    | otherwise = iter

-- function corresponding to blue-green coloured final image
makeCreepyFrac :: Float -> Int -> Int -> PixelRGB8
makeCreepyFrac i x y = PixelRGB8 0 (greenVal x y) (blueVal x y)
    where
      blueVal x y = fromIntegral $ creepyFrac (toCoord i (Point ((fromIntegral (x)), (fromIntegral (y))))) 0 100
      greenVal x y = fromIntegral $ creepyFrac (toCoord i (Point ((fromIntegral (x)), (fromIntegral (y))))) 0 100

-- the following functions add a vaporwave-like colour gradient to the gif
colourHelper :: Int -> Bool
colourHelper iter
    | iter > 50 = True
    | otherwise = False

foregroundGradient :: [Int]
foregroundGradient = concat $ repeat
                ([ round $ (x) * 255 / width | x <- [50..width]]
            ++ [ round $ (x) * 255 / width | x <- [width, (width-1)..50]])

addGradient :: Int -> Int -> Int -> Bool -> PixelRGB8
addGradient n x y hasPixel
    | hasPixel = PixelRGB8 (fromIntegral (foregroundGradient !! (7*n+x))) 150 170
    | otherwise = PixelRGB8 255 199 232

makeVapourwaveCreepyFrac :: Float -> Int -> Int -> Int -> PixelRGB8
makeVapourwaveCreepyFrac i n x y = addGradient n x y $ colourHelper $ creepyFrac ( toCoord i (Point ((fromIntegral (x)), (fromIntegral (y))))) 0 100
