module Main where

import LSystem
import Test.QuickCheck




mandala :: Int -> Command
mandala x = g x :#: p :#: p :#:  p :#: p :#: p :#: p :#: p :#: p :#: p :#: g x :#: p :#:  p :#:  p :#:  p :#:  p :#:  p :#:  p :#: g x
  where 
     f 0 = GrabPen pink :#: Go 10
     f x = f (x-1) :#: f (x-1) :#: Branch (p :#: p :#: f (x-1)) :#: Branch (n :#: n :#: f (x-1))
     g 0 = Go 10 :#: GrabPen orange
     g x = Branch (f (x-1)) :#: Branch (n :#: f (x-1)) :#: Branch (p :#: f (x-1)) :#: Branch (n :#: n :#: f (x-1)) :#: Branch (p :#: p :#: f (x-1)) :#: Branch (n :#: n :#: n :#: f (x-1)) :#: Branch (p :#: p :#: p :#: f (x-1)) :#: Branch (n :#: n :#: n :#: n :#: f (x-1)) :#: Branch (p :#: p :#: p :#: p :#: f (x-1))
     n = Turn 15
     p = Turn (-15)

     -- display (mandala 6)

circle :: Int -> Command
circle x = f :#: f :#: m x :#: f
  where 
     f = GrabPen turquoise :#: Go 10
     m 0 = GrabPen pink :#: Go 10
     m x = m (x-1) :#: p :#: f :#: n :#: m (x-1) :#: n :#: f
     p = Turn (-36)
     n = Turn 36
   -- display (cirle 10)

star :: Int -> Command
star x = f x
    where 
       f 0 = GrabPen purple :#: Go 10
       f x = p :#: f (x-1) :#: n :#: f (x-1) :#: p
       p = Turn (-30)
       n = Turn 30

      -- display (star 8)

squares :: Int -> Command
squares x = m x
 where
    m 0 = GrabPen pink :#: Go 10
    m x = m (x-1) :#: g (x-1) :#: p :#: y (x-1) :#: f x :#: p
    y 0 = GrabPen red :#: Go 10
    y x = n :#: f x :#: m (x-1) :#: n :#: y (x-1) :#: g (x-1)
    g 0 = GrabPen purple :#: Go 10
    g x = m (x-1) :#: y (x-1)
    f x = GrabPen pink :#: Go 10
    n = Turn 90
    p = Turn (-90)
-- display (squares 9)

snowflakestars :: Int -> Command
snowflakestars x = f x :#: n :#: n :#: f x :#: n :#: n  :#: f x :#: n :#:  n :#: f x :#: n :#: n :#: f x 
  where
     f 0 = GrabPen turquoise :#: Go 10
     f x = f (x-1) :#: n :#: f (x-1) :#: p :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1)
     n = Turn 72
     p = Turn (-72) 

     -- display (snowflakestar 4)

spikystar :: Int -> Command
spikystar x = n :#: f x :#: p :#: f x
 where 
    f 0 = GrabPen pink :#: Go 10
    f x = f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: n
    n = Turn 80
    p = Turn (-80)
-- display (spikystar 5)


swirl :: Int -> Command
swirl x = Branch (f x) :#: p :#: p :#: Branch (f x) :#: p :#: p :#: Branch (f x) :#: p :#: p :#: Branch (f x) :#: p :#: p :#: Branch (f x)
 where
    g 0 = GrabPen turquoise :#: Go 10
    g x = y (x-1) :#: l :#: p :#: p :#: d (x-1) :#: l :#: n :#: n :#: n :#: f (x-1) :#: l :#: Branch (n :#: y (x-1) :#: l :#: n :#: n :#: n :#: n :#: g (x-1) :#: l) :#: p :#: p
    f 0 = GrabPen purple :#: Go 10
    f x = p :#: y (x-1) :#: l :#: n :#: n :#: d (x-1) :#: l :#: Branch (n :#: n :#: n :#: g (x-1) :#: l :#: n :#: n :#: f (x-1) :#: l) :#: p
    y 0 = GrabPen pink :#: Go 10
    y x = n :#: g (x-1) :#: l :#: p :#: p :#: f (x-1) :#: l :#: Branch (p :#: p :#: p :#: y (x-1) :#:l :#: p :#: p :#: d (x-1) :#: l) :#: p
    d 0 = Go 10
    d x = n :#: n :#: y (x-1) :#: l :#: p :#: p :#: p :#: p :#: g (x-1) :#: l :#: Branch (p :#: d (x-1) :#: l :#: p :#: p :#: p :#: p :#: f (x-1) :#: l) :#: n :#: n :#: f (x-1) :#: l
    l = Go 10
    n = Turn 36
    p = Turn (-36)

-- display (swirl 4)

crosses :: Int -> Command
crosses x = f x :#: n :#: f x :#: n :#:  f x :#: n :#: f x :#: n :#: f x
  where
     f 0 = GrabPen blue :#: Go 10
     f x = f (x-1) :#: n :#: f (x-1) :#: p :#: p :#: f (x-1) :#: n :#: p :#: f (x-1) :#: n :#: p :#: f (x-1) :#: n :#: f (x-1)
     n = Turn 90
     p = Turn (-90)

-- display (crosses 5)

main :: IO ()
main = displayAll
  [ mandala 6 ~> "mandala"
  , circle 10 ~> "circle"
  , star 8 ~> "star"
  , squares 9 ~> "squares"
  , snowflakestars 4 ~> "snowflakestars"
  , spikystar 5 ~> "spikystar"
  , swirl 4 ~> "swirl"
  , crosses 5 ~> "crosses"
  ]
  where (~>) = flip (,)
