module ConGoLDisp (
  display,
  Command (..),
  Pen (..), black,
  Distance, Angle, cmdToString, displayWithSquares,
  copy, join
) where

import Control.Monad (liftM2)

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

data Pen = Colour Float Float Float
         | Inkless
           deriving (Eq, Ord, Show)

black :: Pen
black = Colour 0.5 0.5 0.5

data Ln = Ln Pen Pnt Pnt
  deriving (Eq,Ord,Show)

type Angle    = Float
type Distance = Float
type Turtle   = (Pen,Angle,Pnt)

data Command = Go Distance
             | Turn Angle
             | Sit
             | Command :#: Command
             | Branch Command
             | GrabPen Pen
               deriving (Eq, Ord, Show)

execute :: Command -> [Ln]
execute c  =  lines
  where
  (lines, _)  =  f c (black, 0, Pnt 0 0)

  f :: Command -> Turtle -> ([Ln], Turtle)
  f (c :#: d) turtle             =  (clines ++ dlines, dturtle)
                                    where
                                    (clines, cturtle) = f c turtle
                                    (dlines, dturtle) = f d cturtle
  f (Branch c) turtle            =  (clines, turtle)
                                    where
                                    (clines, _) = f c turtle
  f (Go dst) (pen,ang,pnt)       =  ( [Ln pen pnt endpnt | pen /= Inkless], (pen,ang,endpnt))
                                    where
                                    endpnt = pnt + scalar dst * polar ang
  f (Turn delta) (pen,ang,pnt)   =  ([], (pen,ang-delta,pnt))
  f (GrabPen new) (_,ang,pnt)  =  ([], (new,ang,pnt))
  f Sit turtle                 =  ([], turtle)

rescale :: [Ln] -> [Ln]
rescale lines | null points  = []
              | otherwise    = map f lines
  where
    f (Ln pen p q)  =  Ln pen (g p) (g q)
    g p             =  swap ((p - p0) / s)
    points          =  [ r | Ln _ p q <- lines, r <- [p, q] ]
    hi              =  foldr1 lub points
    lo              =  foldr1 glb points
    s               =  scalarMax (hi - lo) * scalar 0.55
    p0              =  (hi + lo) * scalar 0.5
    swap (Pnt x y)  =  Pnt y x

polar :: Angle -> Pnt
polar ang  =  Pnt (cos radians) (sin radians)
  where
  radians  =  ang * 2 * pi / 360

parseLine :: Ln -> String
parseLine (Ln (Colour r g b) (Pnt x1 y1) (Pnt x2 y2))
  =
    "\n<line x1=\"" ++ x1' ++ "%\"" ++
          "y1=\"" ++ y1' ++ "%\"" ++
          "x2=\"" ++ x2' ++ "%\"" ++
          "y2=\"" ++ y2' ++ "%\"" ++
          "style=\"stroke:rgb" ++ rgb ++ "\" />"
  where
    x1' = show $ scaleToSvg x1
    y1' = show $ scaleToSvg (-y1)
    x2' = show $ scaleToSvg x2
    y2' = show $ scaleToSvg (-y2)
    rgb = "(" ++ show (r*255) ++ "," ++ show (g*255) ++"," ++ show (b*255) ++ ")"

scaleToSvg x =  (x + 1) / 2 * 100.0

toHtmlBody :: Command -> String
toHtmlBody = concatMap parseLine . rescale . execute

wrapSvgTag:: String -> String
wrapSvgTag x = "<svg height=\"" ++ "100%" ++ "\" width=\"" ++ "100%" ++ "\"style=\"background-color:#707070\"" ++ "\">" ++ x ++ "</svg>"

cmdToString :: Command -> String
cmdToString x = wrapSvgTag(toHtmlBody x)

display :: Command -> IO ()
display x = writeFile "output.html" ((wrapSvgTag (toHtmlBody x)) ++ "Hellp")

displayWithSquares :: Command -> String -> IO ()
displayWithSquares x y = writeFile "output.html" ((wrapSvgTag ((toHtmlBody x)++y)))

join :: [Command] -> Command
join [] = Sit
join (x:[]) = x -- (This line was added to make Ex. 4 Easier)
join (x:xs) = x :#: join xs

copy :: Int -> Command -> Command
copy num com
  | num > 0 = join (replicate num com)
  | otherwise = Sit
