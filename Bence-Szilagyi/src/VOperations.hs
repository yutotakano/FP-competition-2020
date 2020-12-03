module VOperations where

import Linear

import VConversions

-- These could probably be generalised by using the Foldable typeclass and type variables, but alas, time is of the essence
dotV2 :: V2 Float -> V2 Float -> Float
dotV2 v1 v2 = sumV2Float (v1 * v2)

dotV2Sq :: V2 Float -> Float
dotV2Sq v = dotV2 v v

lengthV2 :: V2 Float -> Float
lengthV2 v = sqrt(dotV2 v v)

dotV3 :: V3 Float -> V3 Float -> Float
dotV3 v1 v2 = sumV3Float (v1 * v2)

dotV3Sq :: V3 Float -> Float
dotV3Sq v = dotV3 v v

lengthV3 :: V3 Float -> Float
lengthV3 v = sqrt(dotV3 v v)

mkUnitV2 :: V2 Float -> V2 Float
mkUnitV2 v = v ^* (1 / lengthV2 v)

mkUnitV3 :: V3 Float -> V3 Float
mkUnitV3 v = v ^* (1 / lengthV3 v)
