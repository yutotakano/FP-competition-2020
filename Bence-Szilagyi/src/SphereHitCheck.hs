module SphereHitCheck where

import Linear
import Data.Word
import Data.Foldable
import Data.Complex

import Ray
import VOperations
import Materials
import Fractals

data Sphere = Sph { radius :: Float, center :: Point3D, material :: Material, fractalInit :: (Complex Float, Complex Float, Complex Float, Complex Float) } | NoSphere
    deriving Eq

-- Origin point for the camera
originPoint :: Point3D
originPoint = V3 0.0 0.0 (-0.5)

sphereList :: [Sphere]
sphereList = [Sph 5    (V3 0 (0) (-14))     (M (V4 (178/255) (178/255) (178/255) 1) "metal")   (setA, setB, setC, setD),
              Sph 4    (V3 (-3) 4.5 (4))    (M (V4 (240/255) (191/255) (38/255)  1) "metal")   (setA, setB, setC, setD),
              Sph 7    (V3 (8) 7.5 (12))    (M (V4 (210/255) (159/255) (198/255) 1) "metal")   (setA, setB, setC, setD),
              Sph 1000 (V3 0 (-1005) (-14)) (M (V4 (178/255) (178/255) (178/255) 1) "diffuse") (setA, setB, setC, setD),
              Sph 3    (V3 (-6) (5) (-14))  (M (V4 (210/255) (159/255) (198/255) 1) "metal")   (setA2, setB2, setC2, setD2),
              Sph 2    (V3 (-9) 8 (-14))    (M (V4 (240/255) (191/255) (38/255)  1) "metal")   (setA, setB, setC, setD),
              Sph 1    (V3 9 1 (-14))       (M (V4 (210/255) (159/255) (198/255) 1) "metal")   (setA, setB, setC, setD)]


              
hitSphere :: Sphere -> Ray -> Float
hitSphere sph ray | d == 0 && t1 > 0.1            = t1
                  | d > 0 && t1 > 0.1 && t2 > 0.1 = min t1 t2
                  | otherwise                     = -1.0
    where
        oc = origin ray - center sph
        a  = dotV3Sq $ direction ray
        b  = dotV3 oc $ direction ray
        c  = dotV3Sq oc - radius sph * radius sph
        d  = b * b - a * c
        t1 = (-b - sqrt d) / a
        t2 = (-b + sqrt d) / a
