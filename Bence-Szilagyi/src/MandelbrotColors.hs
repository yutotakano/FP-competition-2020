module MandelbrotColors where

import Linear
import Data.Word

-- I have no idea

a :: Double
b :: Double
c :: Double
rmaj :: Double
rmin :: Double
g :: Double
h :: Double
q :: Double
a = 176
b = 176
c = 176
rmaj = 88
rmin = 88
g = 146
h = -32
q = 0

u :: Double
v :: Double
w :: Double
x :: Double
y :: Double
z :: Double
e :: Double
x1 :: Double
y1 :: Double
z1 :: Double
x2 :: Double
y2 :: Double
z2 :: Double
e' :: Double
f :: Double
x1' :: Double
y1' :: Double
z1' :: Double
u = cos(g * pi / 180) * cos(h * pi / 180)
v = cos(g * pi / 180) * sin(h * pi / 180)
w = sin(g * pi / 180)
x = -a
y = -b
z = a * u / w + b * v / w
e = sqrt(x * x + y * y + z * z)
x1 = x / e
y1 = y / e
z1 = z / e
x2 = v * z1 - w * y1
y2 = w * x1 - u * z1
z2 = u * y1 - v * x1
e' = cos(q * pi / 180)
f = sin(q * pi / 180)
x1' = e' * x1 + f * x2
y1' = e' * y1 + f * y2
z1' = e' * z1 + f * z2
x2' :: Double
x2' = v * z1' - w * y1
y2' :: Double
y2' = w * x1' - u * z1'
z2' :: Double
z2' = u * y1' - v * x1'

pixelColorChannel :: String -> Double -> Word8
pixelColorChannel abc i = round (abc' abc + e'' i * xyz1 abc + f' i * xyz2 abc)
    where
        abc' abc | abc == "a" = a
                 | abc == "b" = b
                 | abc == "c" = c
        xyz1 abc | abc == "a" = x1'
                 | abc == "b" = y1'
                 | abc == "c" = z1'
        xyz2 abc | abc == "a" = x2'
                 | abc == "b" = y2'
                 | abc == "c" = z2'
        e'' i = rmaj * cos(i * pi / 360)
        f' i = rmin * sin(i * pi / 360)