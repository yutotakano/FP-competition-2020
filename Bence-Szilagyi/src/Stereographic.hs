module Stereographic where

import Data.Foldable ( Foldable(toList) )
import Linear ( (*^), V2(..), V3(..) )

import VConversions ( listToV3 )
import VOperations ( dotV3Sq )
import SphereHitCheck ( Sphere(radius, center) )

-- Assuming unit sphere is at origin

infinityPoint :: V2 Float
infinityPoint = V2 0 0

stereographicUnitSphereToPlaneZMinus1 :: V3 Float -> V2 Float
stereographicUnitSphereToPlaneZMinus1 v = 2 *^ V2 (head l * t) (head (tail l) * t)
    where
        n = dotV3Sq v + 0.001
        l = toList v
        t = 1 / (last l - 1)

stereographicPlaneZMinus1ToUnitSphere :: V2 Float -> V3 Float
stereographicPlaneZMinus1ToUnitSphere v = undefined

sgSP :: Sphere -> V3 Float -> V2 Float
sgSP sph v = stereographicUnitSphereToPlaneZMinus1 (1 / radius sph *^ (v - center sph))

sgPS :: Sphere -> V2 Float -> V3 Float
sgPS sph v = radius sph *^ stereographicPlaneZMinus1ToUnitSphere v + center sph
