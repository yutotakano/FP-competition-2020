module Ray where

import Linear
import Data.Word

import Color

-- The function 'origin' returns the origin point of our ray, while the function 'direction' returns the direction vector of our ray.
-- Both are stored in a 3-dimensional vector from the Linear library.
data Ray = Ray { origin :: Point3D, direction :: V3 Float }
    deriving Show

type Point3D = V3 Float

-- Calculate a position along the line of the ray where the distance from the ray's origin
-- is a parameter times the ray's direction vector
pAtParam :: Float -> Ray -> Point3D
pAtParam param ray = origin ray + param *^ direction ray
