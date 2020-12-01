module Greeble (greebleQuad) where

import Vis
import Linear
import Linear.Metric (normalize)
import Util (getRandomSeq)

-- The maximum extrusion in world units
maxExtrusion :: Float
maxExtrusion = 0.5

-- The vector midway between 2 vectors
btwn :: V3 Float -> V3 Float -> V3 Float
btwn a b = a + ((b - a) / 2)

-- Split a quad into four more faces
splitQuad :: VisObject Float -> [VisObject Float]
splitQuad (Quad p1 p2 p3 p4 cl) = [Quad p1 (btwn p1 p2) (btwn p1 p3) (btwn p1 p4) cl,
                                  Quad (btwn p1 p2) p2 (btwn p2 p3) (btwn p2 p4) cl,
                                  Quad (btwn p1 p3) (btwn p2 p3) p3 (btwn p3 p4) cl,
                                  Quad (btwn p1 p4) (btwn p2 p4) (btwn p3 p4) p4 cl]

-- Converts a flat quad to a 3D box by extending it an amount along the given normal
toBoxAlongNormal :: V3 Float -> Float -> VisObject Float -> VisObject Float
toBoxAlongNormal normal' l q@(Quad p1 p2 p3 p4 cl) = VisObjects [Quad p1 p2 p3 p4 cl',
                                                                   Quad p1 p1' p2' p2 cl',
                                                                   Quad p2 p2' p3' p3 cl',
                                                                   Quad p3 p3' p4' p4 cl',
                                                                   Quad p4 p4' p1' p1 cl',
                                                                   Quad p1' p2' p3' p4' cl']
        where normal = normalize normal'
              ext = normal * V3 l l l
              p1' = p1 + ext
              p2' = p2 + ext
              p3' = p3 + ext
              p4' = p4 + ext
              cl' = cl * greyN (max 0.1 (l / maxExtrusion))

-- Greeble a quad, by subdividing it n times then extruding each subdivision along the given normal
-- Magnitude scales the extrusion amount, and seed is used as the starting seed.
greebleQuad :: Int -> Int -> Float -> V3 Float -> VisObject Float -> VisObject Float
greebleQuad n seed magnitude normal q@(Quad _ _ _ _ _) = VisObjects $ map extendRandomly $ zip [0..] (subdivide n [q])
            where subdivide 0 qs = qs
                  subdivide n qs = subdivide (n - 1) $ concat $ map splitQuad qs
                  extendRandomly (i, q) = toBoxAlongNormal normal (randomSeq!!i * magnitude) q
                  randomSeq = getRandomSeq (0, maxExtrusion) (4 ^ n) seed