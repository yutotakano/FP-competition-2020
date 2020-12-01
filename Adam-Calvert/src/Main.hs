module Main where

import Data.Fixed --modulo of floats (used for equidistibution)
import Data.Vect --vector calculations
import Graphics.Gloss.Export.PNG --allows the exporting of a gloss image to file
import Graphics.Gloss.Raster.Field --allows the writing of pixels to an image
import System.CPUTime --timing frame intervals
import System.Directory --file handling
--import Test.QuickCheck --once used for testing
import Text.Printf --better console printing

main :: IO ()
main = do
  folderIndex <- getEmptyFolder 0
  createDirectory (".\\output" ++ show folderIndex ++ "\\")
  putStrLn "Commencing rendering!"
  printf "Rendering frame %d!\n" ((head frames) :: Int)
  mapM_ --IO is.. interestings
    ( \(i, j) -> do
        start <- getCPUTime
        exportPictureToPNG res black (".\\output" ++ show folderIndex ++ "\\" ++ (zeroPad (show i) maxLength) ++ ".png") (uncurry makePicture res 1 1 (getPixel ratio (fromIntegral i / fromIntegral fps)))
        end <- getCPUTime
        let diff = fromIntegral (end - start) / (10 ^ 13) --might be incorrect
        printf "Rendering frame %d! (Last frame time %0.3fs!)\n" (j :: Int) (diff :: Double)
    )
    (zipAdj frames) --operate on pairs of frames, so we can get frame intervals
  return ()
  where
    res = (1886, 1061) --1061 is a limit forced by gloss-export, so we set resolution to 1886x1061 and then downscale to 1280x720 (this achieves a psuedo- supersampling effect)
    frames = equidistribution framecount --shuffle frames
    framecount = fps * duration
    duration = 30
    fps = 60
    zipAdj x = zip x $ tail x
    ratio = (\(x, y) -> fromIntegral x / fromIntegral y) res
    maxLength = maximum $ map (length . show) frames

--get first available folder recursively
getEmptyFolder :: Int -> IO Int
getEmptyFolder n = do
  dirExists <- doesDirectoryExist (".\\output" ++ show n ++ "\\")
  if dirExists
    then getEmptyFolder $ succ n
    else return n

--zeropad numeric string to given length
zeroPad :: String -> Int -> String
zeroPad str n
  | length str > n = str
  | otherwise = replicate (n - length str) '0' ++ str

--------------------

--gets color of pixel at given point on screen at given time value (also takes aspect ratio, to correct camera rays)
getPixel :: Float -> Float -> Point -> Color
getPixel r t p
  | rayCollides = colorFromVec3 (vec3FromColor color &* ao)
  | otherwise = black
  where
    ray = getRay r p
    origin = Vec3 0.0 0.0 (-5.0)
    (rayCollides, dists) = marchRay (distanceEstimator t) ray origin iterations (True, [])
    ao = remap (0.0, 1.0) (-1.0, 1.0) (1.0 - (fromIntegral (length dists) / fromIntegral iterations)) ** 2 --raymarching essentially gives us free ambient occlusion, i'm using it for shading (there are better ways of doing this)
    hit = origin &+ (ray &* sum dists)
    color = mixColors (remap (-1.0, 1.0) (0.0, 1.0) (sin (len hit * 6))) (remap (-1.0, 1.0) (0.0, 1.0) (-1 * sin (len hit * 6))) red orange --determine color based on distance from origin (there are better ways of doing this)
    iterations = 256

--converts vec3 to gloss color
colorFromVec3 :: Vec3 -> Color
colorFromVec3 (Vec3 r g b) = rgb r g b

--converts gloss color to vec3
vec3FromColor :: Color -> Vec3
vec3FromColor c = Vec3 r g b
  where
    (r, g, b, _) = rgbaOfColor c

--remaps a float from one range into another
remap :: (Float, Float) -> (Float, Float) -> Float -> Float
remap (a, b) (c, d) t = c + (t - a) * (d - c) / (b - a)

--maps a pixel to a view ray
getRay :: Float -> Point -> Vec3
getRay r (x, y) = normalize (Vec3 (x * r) y z)
  where
    z = 1.0 / tan (degToRad (fov / 2.0) / 2.0)
    fov = 55.0
    degToRad d = d * pi / 180

--marches a ray across a scene
--http://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/
marchRay :: (Vec3 -> Float) -> Vec3 -> Vec3 -> Int -> (Bool, [Float]) -> (Bool, [Float])
marchRay de ray origin n (rayCollides, dists)
  | currentDist < epsilon || length dists == n || sum dists >= maxDistance = (length dists /= n && sum dists < maxDistance, dists)
  | otherwise = marchRay de ray origin n (rayCollides, currentDist : dists)
  where
    epsilon = 1e-4
    maxDistance = 25.0
    currentDist = de (origin &+ (ray &* sum dists))

--converts point to spherical coordinates, raises it to a given power, and converts it back to cartesian coordinates
transformPoint :: (Vec3, Float) -> Vec3 -> Float -> (Vec3, Float)
transformPoint (w@(Vec3 wx wy wz), dr) c p
  | abs wr > 1.5 = (w, dr)
  | otherwise = (Vec3 x y z &+ c, dr')
  where
    wr = len w
    wo = acos (wy / wr)
    wi = atan (wx / wz)
    wr' = wr ** p
    wo' = wo * p
    wi' = wi * p
    x = wr' * sin wo' * sin wi'
    y = wr' * cos wo'
    z = wr' * sin wo' * cos wi'
    dr' = ((wr ** (p - 1.0)) * p * dr) + 1.0

--calculates distance to scene at given point
--used for raymarching
distanceEstimator :: Float -> Vec3 -> Float
distanceEstimator t pos = 0.5 * log r * r / l
  where
    r = len z
    (z, l) = iterate (\w -> transformPoint w pos pow) (pos, 1.0) !! iterations --iterate point (this is what actually makes the mandelbulb)
    pow = 15 * easeQuadratic (t / 30) --scales time with easing function
    iterations = 256

--quadratic easing function
easeQuadratic :: Float -> Float
easeQuadratic x
  | x < 0.5 = 2 * x * x
  | otherwise = 1 - ((-2 * x + 2) ** 2) / 2

--uses mathematical magic to shuffle frames
--https://en.wikipedia.org/wiki/Equidistribution_theorem
--isn't perfect (can repeat or leave out frames), but seems to work for large n
-- allows us to keep a somewhat constant framerate for the whole animation no matter how many frames we have rendered
equidistribution :: Int -> [Int]
equidistribution n = map (\x -> round $ mod' (phi * x) 1 * fromIntegral n) ([1.0 .. (fromIntegral n)] ++ [0.0])
  where
    phi = (1 + sqrt 5) / 2 :: Float

--used for testing
--will fail
--works well enough either way for our purposes
-- ¯\_(ツ)_/¯
-- prop_equidistribtuion :: Int -> Property
-- prop_equidistribtuion n =
--   n >= 0 ==> length (equidistribution n) == succ n
--     && and [x `elem` equidistribution n | x <- [0 .. n]]