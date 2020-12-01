module Materials where

import System.Random
import Linear

import VConversions
import VOperations

data Material = M { reflectivity :: V4 Float, mType :: String } | NoMat
    deriving Eq

sphR :: Int -> StdGen
sphR = mkStdGen

rV3 :: Int -> V3 Float
rV3 i = (2 *^ listToV3 $ take 3 (randomRs (0.0, 1.0) (sphR i))) - V3 1 1 1

getRV3InUnitSphere :: Int -> V3 Float
getRV3InUnitSphere i = if lengthV3 v < 1.0 then v else getRV3InUnitSphere (head (randoms (sphR i) :: [Int]))
    where v = rV3 i

bounce :: Material -> V3 Float -> V3 Float -> Int -> V3 Float
bounce m n dir randSeed | mType m == "diffuse" = n + getRV3InUnitSphere randSeed
                        | mType m == "metal"   = dir - 2 * dotV3 dir n *^ n
