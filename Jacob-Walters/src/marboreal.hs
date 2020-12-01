{-

MARBOREAL - Procedural marble texture generator

Written by: Jacob Walters (s2026618)


To execute, run `make png`. This will generate a file `out.png`, with the
desired output.

Try changing the numbers for scale, x/yRes, and the RNG seed in main to get
different results.


References used:
https://www.iquilezles.org/www/articles/warp/warp.htm

-}

import System.Random
import Data.List (transpose, intercalate, sortBy)
import Data.List.Split (chunksOf)

xRes, yRes :: Int
xRes = 1920
yRes = 1200

scale :: Float
scale = 0.001


-- helper functions for rounding
rd, ru :: Float -> Float
rd = fromIntegral . floor
ru = (1+) . rd

-- Various interpolation polynomials
fade :: Float -> Float
-- fade x = x  -- Linear
-- fade x = 3*x^2 - 2*x^3  -- Smoothstep
fade x = x^3 * (x * (x*6 - 15) + 10)  -- Smootherstep


data Vct = Vct Float Float
    deriving (Eq, Show)

instance Num Vct where
    (Vct a b) + (Vct c d) = Vct (a+c) (b+d)
    (Vct a b) - (Vct c d) = Vct (a-c) (b-d)

-- scalar vector multiplication
sv :: Float -> Vct -> Vct
n `sv` (Vct a b) = Vct (a*n) (b*n)

dot :: Vct -> Vct -> Float
(Vct a b) `dot` (Vct c d) = a*c + b*d

angleToUnitVct:: Float -> Vct
angleToUnitVct theta = Vct (sin theta) (cos theta)

vFst, vSnd :: Vct -> Float
vFst (Vct a b) = a
vSnd (Vct a b) = b

randVss :: Int -> Int -> StdGen -> [[Vct]]
randVss x y = take y . chunksOf x . map (angleToUnitVct . (2*pi*)) . randoms


perlinNoise :: Vct -> [[Vct]] -> Float
perlinNoise v@(Vct x y) vss = (i1-i0)*fy + i0
    where v0 = Vct (rd x) (rd y)
          v1 = v0 + (Vct 1 0)
          v2 = v0 + (Vct 0 1)
          v3 = v0 + (Vct 1 1)
          g (Vct x y) = vss!!(max 0 $ round x)!!(max 0 $ round y)  -- Only ever called on int vects
          d0 = (g v0) `dot` (v - v0) -- Gradient vectors dotted with offset vectors
          d1 = (g v1) `dot` (v - v1)
          d2 = (g v2) `dot` (v - v2)
          d3 = (g v3) `dot` (v - v3)
          fx = fade (x - (rd x))
          fy = fade (y - (rd y))
          i0 = (d1-d0) * fx + d0
          i1 = (d3-d2) * fx + d2

fbm :: Int -> Vct -> [[Vct]] -> Float
fbm octs v vss = go octs
    where go 0    = (perlinNoise v vss)
          go octs = (0.5/2^octs)*(perlinNoise ((2^octs) `sv` v) vss) + go (octs-1)


type VField = [Float]
first, second, third :: VField -> Float
first  [a, b, c] = a
second [a, b, c] = b
third  [a, b, c] = c

warp :: Vct -> [[Vct]] -> VField
warp v@(Vct x y) vss = [(f (v + r) vss), (vFst q), (vSnd r)]
    where f = fbm 3
          q = 4 `sv` (Vct (f v vss)
                          (f (v + (Vct 5.2 1.3)) vss))
          r = 4 `sv` (Vct (f (v+q+(Vct 1.7 9.2)) vss)
                          (f (v+q+(Vct 8.3 2.8)) vss))
-- values here are arbitrary, although these exact ones are taken from
-- https://www.iquilezles.org/www/articles/warp/warp.htm


-- Generate image from randoms
transform :: Float -> [[Vct]] -> [[VField]]
transform sc vss = [[warp (Vct x y) vss
                    | x <- [0.0,sc..sc*(fromIntegral xRes-1)]]
                    | y <- [0.0,sc..sc*(fromIntegral yRes-1)]]



-- Convert floats to ints in [lo,hi]
rescale :: Float -> Float -> [[Float]] -> [[Int]]
rescale lo hi fss = map (map (max 0)) $ map (map floor) scaledfss
    where diff = hi - lo
          minf = minimum $ map minimum fss
          min0 = map (map ((lo +) . subtract minf)) fss
          maxf = maximum $ map maximum min0
          scaledfss = map (map (* (diff/maxf))) min0


data Colour = GS Int
            | RGB Int Int Int
    deriving (Eq, Show)

showCol :: Colour -> String
showCol (GS x)      = show x
showCol (RGB r g b) = intercalate " " $ map show [r, g, b]

brightness :: Colour -> Int
brightness (GS x)      = x
brightness (RGB r g b) = round (fromIntegral (r+g+b)/3)



-- Generate PGM output
mkNetPBM :: [[Colour]] -> String
mkNetPBM css = "P3\n" ++ show xRes ++ " " ++ show yRes ++ "\n" ++
            (show $ maximum $ map (maximum . map (brightness)) css) ++ "\n" ++
            (unlines $ map (unwords . map showCol) css)


genfield :: Int -> Int -> Float -> StdGen -> [[VField]]
genfield x y sc gen = transform (scale/sc)
    $ randVss (round (fromIntegral x/sc)) (round (fromIntegral y/sc)) gen


main :: IO ()
main = do
    let field = genfield xRes yRes 1 (mkStdGen 34987)
    -- values for rescale chosen largely arbitrarily
    let rs = rescale 0 191 $ map (map first)  field
    let gs = rescale 0 127 $ map (map second) field
    let bs = rescale 0 127 $ map (map third)  field
    let cols = zipWith3 (zipWith3 RGB) rs gs bs
    putStr $ mkNetPBM cols
