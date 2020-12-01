import Data.Function (on)
import Data.List (findIndex, groupBy)
import Data.Ratio ((%))
import Numeric (showHex)
import System.Environment (getArgs)

type N = Integer
type Q = Rational

twice :: Q -> Q
twice p
   | q >= 1 = q - 1
   | otherwise = q
   where q = 2 * p

rationals :: [Q]
rationals =
   [ q
   | (per, d) <- denom
   , n <- [ 1 .. d - 1 ]
   , let q = n % d
   , period q == per ]

denom :: [(N, N)]
denom = iterate (\(per, den) -> (per + 1, 2 * den + 1)) (1, 1)

period :: Q -> N
period p =
   let Just e = (p ==) `findIndex` drop 1 (iterate twice p)
   in  fromIntegral e + 1

-- Lavaurs' algorithm:
-- (1) Connect 1/3 and 2/3,
-- (2) Assuming all the numbers of period < k have been connected, 
--connect those of period k, 
--starting with the smallest number not yet connected, 
--and connecting it to the next smallest number not yet connected, 
--always making the choices so no connecting arc crosses any other arc.

mandel :: [(N, (Q, Q))]
mandel = (2, (1%3, 2%3)) : mandel' (drop 2 rationals)
 
 
intersects :: (Q, Q) -> (Q, Q) -> Bool
intersects (a, q) (s, v)
   | q < s = False
   | v < a = False
   | a < s && v < q = False
   | s < a && q < v = False
   | otherwise = True

free :: Q -> Q -> Bool
free p q = null
               . filter (intersects (p, q) . snd)
               . takeWhile ((< period p) . fst)
               $ mandel
 
mandel' :: [Q] -> [(N, (Q, Q))]
mandel' (p : ps) =
   let (qs, r : rs) = break (free p) ps
   in  (period p, (p, r)) : mandel' (qs ++ rs)
mandel' [] = error "broken invariant"


-- the goal was to produce an animation but running main in the terminal 
-- gave me this error: ghc[65825:3317749] GLUT Fatal Error: internal error: NSInternalInconsistencyException, 
---reason: NSWindow drag regions should only be invalidated on the Main Thread!
-- so the following code renders a svg


main :: IO ()
main = do
   [n] <- map read `fmap` getArgs
   putStr header
   putStr . concatMap g . take n . groupBy ((==) `on` fst) $ mandel
   putStr footer
   where
     header = unlines
       [ "<?xml version='1.0' encoding='UTF-8'?>"
       , "<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>"
       , "<svg xmlns='http://www.w3.org/2000/svg' width='4096' height='4096' viewbox='0 0 4096 4096'>"
       , "<title>Combinatorics in the Mandelbrot Set - Lavaurs Algorithm</title>"
       , "<rect x='0' y='0' width='4096' height='4096' stroke='none' fill='#1e002a' />"
       , "<g stroke-width='2.5' fill='none'>"
       ]
     footer = unlines
       [ "</g>"
       , "<circle cx='2048' cy='2048' r='2000' fill='none' stroke='black' stroke-width='1' />"
       , "</svg>"
       ]
     g npqs = let (n:_, pqs) = unzip npqs
              in  "  <g stroke='" ++ colours !! fromIntegral n ++ "'>\n" ++ concatMap arc pqs ++ "  </g>\n"
     arc (p, q) = "    <path d='M" ++ px ++ "," ++ py ++ "A" ++ r ++ "," ++ r ++ " 0 0 1 " ++ qx ++ "," ++ qy ++ "' />\n"
       where
         pa = 2 * pi * fromRational p
         qa = 2 * pi * fromRational q
         r  = s $ 2000 * tan ((qa - pa) / 2)
         px = s $ 2048 + 2000 * cos pa
         py = s $ 2048 - 2000 * sin pa
         qx = s $ 2048 + 2000 * cos qa
         qy = s $ 2048 - 2000 * sin qa
         s :: Double -> String
         s = show

data RGB = RGB Double Double Double
 
-- i was facing issues despite installing packages for colour 
-- specifically the data.colour.palette.colourset so the colours below are 
-- made aesthetically pleasing using the application of golden ratio 

colours :: [String]
colours = map (rgbToSVG . hueToRGB) [0, 2 * pi / (filler * filler) ..]
   where
     filler = (sqrt 5 + 1) / 2
     hueToRGB h =
       let s = 1
           v = 1
           hh = h * 3 / pi
           i = floor hh `mod` 6 :: Int
           f = hh - fromIntegral (floor hh :: Int)
           p = v * (1 - s)
           q = v * (1 - s * f)
           t = v * (1 - s * (1 - f))
       in  (case i of
             0 -> RGB v t p
             1 -> RGB q v p
             2 -> RGB p v t
             3 -> RGB p q v
             4 -> RGB t p v
             _ -> RGB v p q)
 
rgbToSVG :: RGB -> String
rgbToSVG (RGB r g b) =
   let toInt x = let y = floor (256 * x) in 0 `max` y `min` 255 :: Int
       hexadecpt2 i = (if i < 16 then ('0':) else id) (showHex i "")
   in  '#' : concatMap (hexadecpt2 . toInt) [r,g,b]



