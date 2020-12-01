module Main (main) where

import Greeble
import Linear
import Vis
import Data.Time.Clock.POSIX
import Util (timeToMagnitude, boxToQuads)

-- The time, in seconds, to repeat the animation after.
period :: Float
period = 5.0

-- The amount of times to subdivide. 4 is as much as my laptop can handle :/
divisions :: Int
divisions = 4

-- Renders our animation
main :: IO ()
main = do 
        time <- getPOSIXTime
        let seed = floor time
        animate options (greebleAnim divisions seed)

-- Options for the window
options :: Options
options = Options Nothing Nothing Nothing "greeble" Nothing Aliased

-- Generates a function to animate with
-- n is the number of times to subdivide, s is the random seed, then t is the time elapsed.
greebleAnim :: Int -> Int -> Float -> VisObject Float
greebleAnim n s t = Trans (V3 (-2.5) (-2.5) (-2.5)) $ VisObjects $
                        map (uncurry (greebleQuad n s (timeToMagnitude period t))) $
                            boxToQuads (Box (5, 5, 5) Solid (makeColor (213 / 255) (0 / 255) (0 / 255) 1))
