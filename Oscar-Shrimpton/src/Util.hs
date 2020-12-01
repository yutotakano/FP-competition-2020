module Util (getRandomSeq, timeToMagnitude, boxToQuads) where

import System.Random
import Data.Fixed (mod')
import Linear
import Vis

-- Gets an n long sequence of random numbers in the given range
-- seed is used as the initial generator seed
getRandomSeq :: (Float, Float) -> Int -> Int -> [Float]
getRandomSeq range n seed = getRandom n (mkStdGen seed)
    where getRandom 0 g = [fst $ randomR range g]
          getRandom n g = fst out : getRandom (n - 1) (snd out)
            where out = randomR range g

-- Converts a time to a magnitude using a sin wave
-- Returns a float from 0 to 1, which is 0 when t is a multiple of period or and 1 when t is a multiple of (period / 2)
timeToMagnitude :: Float -> Float -> Float
timeToMagnitude period t = sin $ (t `mod'` period) / period * pi

-- Converts a Box object to an array of quads and normals for those quads.
boxToQuads :: VisObject Float -> [(V3 Float, VisObject Float)]
boxToQuads (Box (l, w, h) _ cl) = [(V3 0.0 0.0 (-1.0), Quad (V3 0 0 0) (V3 l 0 0) (V3 l w 0) (V3 0 w 0) cl),
                                            (V3 0.0 0.0 1.0, Quad (V3 0 0 h) (V3 l 0 h) (V3 l w h) (V3 0 w h) cl),
                                            (V3 0.0 (-1.0) 0.0, Quad (V3 0 0 0) (V3 0 0 h) (V3 l 0 h) (V3 l 0 0) cl),
                                            (V3 1.0 0.0 0.0, Quad (V3 l 0 0) (V3 l 0 h) (V3 l w h) (V3 l w 0) cl),
                                            (V3 0.0 1.0 0.0, Quad (V3 l w 0) (V3 l w h) (V3 0 w h) (V3 0 w 0) cl),
                                            (V3 (-1.0) 0.0 0.0, Quad (V3 0 w 0) (V3 0 w h) (V3 0 0 h) (V3 0 0 0) cl)]
boxToQuads _ = error ""