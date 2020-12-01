module Fractals where

import Codec.Picture

import LineGraphics

-- move the origin of the line to  a given point
startLineFrom :: Point -> Line -> Line
startLineFrom (x0, y0) ((xS, yS), (xE, yE))
  = ((x0, y0), (x0 + xE - xS, y0 + yE - yS)) 

-- move the second line so it starts at the endpoint of the first line
connectLine :: Line -> Line -> Line
connectLine l1 l2 
  = startLineFrom (snd l1) l2

-- scale the length of the line by the given factor;
-- point of origin is the same as the input line
scaleLine :: Float -> Line -> Line
scaleLine f ((x1, y1), (x2, y2))
  = ((x1, y1), (x1 + (x2 -x1) * f , y1 + (y2 - y1) * f))

-- rotate line by given angle about the origin
rotateLine :: Float -> Line -> Line
rotateLine alpha l@(p1@(x1, y1), p2@(x2, y2)) 
  = ((x1, y1), ((cos   alpha) * nx - (sin  alpha) * ny + x1,
     (sin  alpha) * nx +(cos  alpha) * ny + y1))
  where
    (nx, ny) = (x2 - x1, y2 - y1)

fade :: Colour -> Colour
fade (r, y, b, o)
  | o == 0    = (r, y, b, 0)
  | otherwise = (r, y - 0, b - 0,  o - 5)

spiralRays :: Float -> Float -> Int -> Colour -> Line -> Picture
spiralRays angle scaleFactor n colour line
  = spiralRays' n colour line
  where
    spiralRays' n colour line@(p1, p2)
      | n <= 0 = []
      | otherwise = (colour, [p1, p2]) : spiralRays' (n-1) newColour newLine
      where
        newColour = if (n `mod` 3 == 0) then fade colour else colour
        newLine   = scaleLine scaleFactor (rotateLine angle line)

spiral :: Float -> Float -> Int -> Line -> Path
spiral angle scaleFactor n line
  = spiralR' n line
  where
    spiralR' n line@(p1, p2)
      | n <= 0    = []
      | otherwise = p1 : spiralR' (n-1) newLine
      where
        newLine = connectLine line 
                    (scaleLine scaleFactor (rotateLine angle line))

polygon :: Int -> Line -> Path
polygon n line | n > 2 = spiral rotationAngle 1 (n + 1) line
  where 
    rotationAngle = (2 * pi) / (fromIntegral n)

-- constructs the tree as one single path
fractalTree :: Float -> Int -> Line -> Path
fractalTree factor n line = fractalTree' n line
  where
    fractalTree' 0 line = []  
    fractalTree' n line 
      = [p1, p4] ++ fractalTree' (n-1) (p4,p5) ++
                    fractalTree' (n-1) (p5,p3) ++
        [p3,p2] 
      where 
        flipLine (pS, pE) = (pE, pS)
        [p1,p2,p3,p4,_]   = polygon 4 line
        (_, p5)           = rotateLine (factor * pi) $ 
                              flipLine $ 
                                scaleLine 0.5 $ (p3, p4)

-- Main function
main :: IO ()
main = do {
  writePng "trial/blueSpiral.png" (drawPicture 2 (spiralRays (0.04) 0.99 400 blue line2)) >>
  writePng "trial/greenSpiral.png" (drawPicture 2 (spiralRays (pi/1.23) 0.95 400 green line2)) >>
  writePng "trial/redShape.png" (drawPicture 1 [(red, spiral (pi/3.5 + 0.1) 1.02 180 line1)]) >>
  writePng "trial/orangeSpiral.png" (drawPicture 2 (spiralRays (pi/22.0) 1.02 400 orange line1)) >>
  writePng "trial/magentaSpiral.png" (drawPicture 3 (spiralRays (pi/1.01) 0.97 400 magenta line2)) >>
  writePng "trial/darkBlueSpiral.png" (drawPicture 2 (spiralRays (0.8342) 1.02 400 darkblue line1)) >>
  writePng "trial/orangeShape.png" (drawPicture 1 [(orange, spiral (pi + 0.1) 1.04 180 line1)]) >>
  writePng "trial/lightBlueTree.png" (drawPicture 1 [(lightgreen, fractalTree 0.35 13 line3)]) >>
  writePng "trial/whiteTree.png" (drawPicture 1 [(white, fractalTree 0.45 13 line3)])
}

-- My lines
line1 :: Line
line1 = ((400, 400), (420, 420))

line2 :: Line
line2 = ((400, 400), (620, 620))

line3 :: Line
line3 = ((400, 700), (300, 700))

-- For animation, unused:
-- ffmpeg -framerate 10 -pattern_type glob -i '*.png' -vcodec libx264 -s 640x480 \-pix_fmt yuv420p movie.mp4

-- Adapted from Haskell For Mac Fractals