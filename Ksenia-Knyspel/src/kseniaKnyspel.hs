module Einstein where

import LSystem

-- drawing one einstein
copy :: Int -> Command -> Command
copy n c | n <= 0 = Sit
         | n == 1 = c
         | n > 1 = c :#: copy (n-1) c

polygon :: Distance -> Int -> Command 
polygon d n  = copy (n) (Go d :#: Turn (360 / fromIntegral n)) 

chin :: Distance -> Int -> Command 
chin d n  = copy (n `div` 6) (Go d :#: Turn (360 / fromIntegral n)) 
    
einstein :: Pen -> Command
einstein x = GrabPen x :#: Turn (180) :#: Turn (-10) :#: Go 4 :#: Branch (Turn 120 :#: Go 8 :#: Turn (-30) :#: Go 5) :#: Branch (Turn 140 :#: Go 6) :#: Go 6 :#: Branch (Turn 160 :#: Go 6 :#: Turn (-50) :#: Go 4) :#: Go 8 :#: Branch (Turn 170 :#: Go 6 :#: Turn (-20) :#: Go 4 :#: Turn (-30) :#: Go 3):#: Turn (-10) :#: Turn (19) :#: Go 40 :#: Branch (hair x) :#: Turn 26 :#: Go 20 :#: Branch (Turn 170 :#: Go 23) :#: Turn 7 :#: Go 10 :#:
            chin 0.75 100 :#: chin 0.75 75 :#: Go 11 :#: Branch (Turn 20 :#: Go 13) :#: Go 11 :#: Branch (Turn 20 :#: Go 12 :#: (tongue x)) :#: Go 18 :#: Branch (Turn 52 :#: Go 6) :#: 
            Turn 15 :#: Go 16 :#: Turn 20 :#: Go 12 :#: Turn 70 :#: Go 2 :#: Turn 90 :#: Go 2 :#: Branch(Turn (-120) :#: Go 4 :#: Turn (-20) :#: Go 10) :#: Go 11 :#: Turn (-160) :#: Go 8 :#: Branch ( Turn 40 :#: Go 3 :#: Turn (-30) :#: Go 8):#: Turn 158 :#: Go 7 :#: Turn 30 :#: Go 4
    
tongue :: Pen -> Command
tongue x = GrabPen white :#: Turn 90 :#: Go 12 :#: GrabPen x :#: Go 3.5 :#: Turn (-70) :#: Go 3.5 :#: Turn 105 :#: Go 3.5 :#: Turn (-95) :#: Go 3.5 :#: Turn 150 :#: Go 3 :#: Turn (-155) :#: Go 3 :#:
            Turn 140 :#: Go 3 :#: Turn (-120) :#: Go 4.5 :#: Turn 95 :#: Go 4 :#: Turn (-95) :#: Go 3 :#: Turn 115 :#: Go 2.5 :#: Turn (-120) :#: Go 3.5 :#: Turn 155 :#: Go 2 :#: Turn 200 :#: Go 3.5 :#: Branch (nose x) :#:
            Turn 60 :#: Go 2 :#: Turn 80 :#: Go 4 :#: Turn (50) :#: Go 3 :#: Turn (-80) :#: Go 2.5 :#: Turn 80 :#: Go 6 :#: Turn 80 :#: Go 3 :#: Turn (-80) :#: Go 3 :#: Turn 90 :#: Go 3 :#: Turn (-80) :#: Go 2 :#:
            Turn 90 :#: Go 3.5 :#: Turn 180 :#: Go 2 :#: Turn (100) :#: Go 2 :#: Turn 80 :#: Go 3 :#: Turn (-90) :#: Go 2 :#: Turn 90 :#: Go 2 :#: Turn (-80) :#: Go 3 :#:
            Branch (Turn (-65) :#: Go 20 :#: Turn (-2) :#: (copy 15 (Go 0.75 :#: Turn (-10))) :#: Branch (GrabPen white :#: Turn (-90) :#: Go 5 :#: GrabPen x :#: Turn (65) :#: Go 12) :#: Turn (-5) :#: Go 15 :#: (copy 10 (Go 0.78 :#: Turn (-2)))) :#:
            Turn (-65) :#: Go 2 :#: Turn 70 :#: Go 2 :#: Turn 85 :#: Go 3 :#: Turn (-120) :#: Go 3.5 :#: Turn 170 :#: Go 4.5 :#: Turn (-80) :#: Go 3 :#: Turn 105 :#: Go 3.75
   
nose ::Pen -> Command
nose x = GrabPen white :#: Turn (-30) :#: Go 4 :#: GrabPen x :#: Branch (Turn (-5) :#: Go 18 :#:Turn 40 :#: Branch (eyeL x) :#: Go 5) :#: Turn (-130) :#: Go 5 :#: Turn 50 :#: Go 4 :#: Turn 30 :#: Go 2 :#: Turn (-50) :#: Go 4 :#: Turn 70 :#: Go 3 :#: Turn 50 :#: Go 4.5 :#: Turn (-150) :#: Branch (eyeR x) :#: Go 9 
   
eyeL :: Pen -> Command
eyeL x = GrabPen white :#: Turn 65 :#: Go 2 :#: GrabPen x :#: Turn (-45) :#: Go 7 :#: Branch (Turn 40 :#: Go 3.5 :#: Turn 50 :#: Go 2 :#: Branch (GrabPen white :#: Turn (-100) :#: Go 2 :#: GrabPen x :#: Turn (-70) :#: Go 7 :#: Turn (-60) :#: Go 6 :#: 
            Turn (-80) :#: Go 1.5 :#: Turn (-83) :#: Go 9) :#: GrabPen white :#: Turn 10 :#: Go 2 :#: GrabPen x :#: Turn 35 :#: Go 3.5 :#: Turn 30 :#: Go 3 :#: 
            Turn 110 :#: Go 2.5 :#: Turn (-70) :#: Go 3 :#: GrabPen white :#: Turn (-80) :#: Go 2 :#: GrabPen x :#: Turn 110 :#: Go 3) :#: Turn 60 :#: Go 1.5 :#: Turn 40 :#: Go 2 :#: Turn 145 :#: Go 1.5 :#: Turn(-12) :#: Go 1.45 :#: Turn 180 :#: polygon 0.1 100 

eyeR :: Pen -> Command
eyeR x = GrabPen white :#: Turn 130 :#: Go 12 :#: GrabPen x :#: Branch (Turn (-120) :#: Go 4 :#: Turn 50 :#: Go 6 :#: Turn (-60) :#: Go 3) :#: GrabPen white :#: Turn (-30) :#: Go 1.5 :#: GrabPen x :#: Branch (Turn (-90) :#: Go 2.5 :#: Turn 45 :#: Go 2.5) :#:
            Branch (Go 3.5 :#: Turn (-50) :#: Turn (-150) :#: Branch (Turn 25 :#: Go 1 :#: polygon 0.1 100) :#: Turn 150 :#: Go 4 :#: Turn (-50) :#: Go 2.5) :#:
            GrabPen white :#: Turn 90 :#: Go 2 :#: GrabPen x :#: Branch (Turn (-60) :#: Go 3 :#: GrabPen white :#: Turn 5 :#: Go 6 :#: GrabPen x :#: Go 2 :#: Branch (GrabPen white :#: Turn (-35) :#: Go 5 :#: Branch (GrabPen x :#: Turn (-45) :#: Go 7 :#: Turn (-70) :#: Go 8) :#: Go 3 :#: Branch (GrabPen x :#: Turn (-40) :#: Go 4 :#: Turn (-25) :#: Go 4 :#: Turn (-40) :#: Go 3) :#: Turn 37 :#: Go 3.5 :#: GrabPen x :#: Turn (-105) :#: Go 11 :#: Turn (-40) :#: Go 4) :#: 
            Turn (-80) :#: Go 10 :#: Turn (-60) :#: Go 7 :#: Turn (-160) :#: Go 5 :#: Turn 35 :#: Go 10) :#: Turn (-100) :#: Go 5 :#: Turn 110 :#: Go 1.5 :#: Turn (-135) :#: Go 6 :#: Turn (-50) :#: Go 4

hair :: Pen -> Command 
hair x = Turn (-30) :#: Go 3.5 :#: Turn (-30) :#: Go 4 :#: GrabPen white :#: Turn (-80) :#: Go 4 :#: GrabPen x :#: Turn (-120) :#: Go 4 :#: Turn 60 :#: Go 8
            :#: Branch (Turn 28 :#: Go 25 :#: Turn (-43) :#: Go 8) :#: Branch (Turn 34 :#: Go 27 :#: Turn (-44) :#: Go 17.75) :#: Turn 165 :#: Go 5 :#: Turn (-50) :#: Go 4
            :#: GrabPen white :#: Turn (-40) :#: Go 2 :#: Turn (-100) :#: Branch (GrabPen x :#: Go 5 :#: Turn 50 :#: Go 18 :#: Turn 20 :#: Go 8 :#: Turn (-30) :#: Go 6 :#: Turn (-33) :#:
            Go 20.75) :#: GrabPen x :#: Go 2 :#: Turn 95 :#: Go 5 :#: Turn (-50) :#: Go 6 :#: Turn (-20) :#: Go 8 :#: Branch (Turn (-140) :#: Go 2 :#: Turn (-25) :#: Go 7) :#: Turn 40 :#: 
            Go 12 :#: Branch (Turn (-30) :#: Go 7 :#: Turn (-35) :#: Go 21) :#: Go 6 :#: Turn (-50) :#: Go 14 :#:Turn (-32) :#: Go 3 :#: Branch (Go 9.5) :#: Turn 30 :#: Go 5.5 :#: Branch (Turn (-42) :#: 
            Go 6) :#: GrabPen white :#: Turn 20 :#: Go 2 :#: GrabPen x :#: Turn (-40) :#: Go 3 :#: Turn (-73) :#: Go 3.6 :#: Branch (Turn 165 :#: Go 5 :#: Turn (-50) :#: Go 5) :#: 
            GrabPen white :#: Turn 60 :#: Go 3 :#: GrabPen x :#: GrabPen x :#: Turn 80 :#: Go 3 :#: Branch (Turn 50 :#: Go 3 :#: Turn (-60) :#: Go 4) :#: Branch (Go 5 :#: Turn (-30) :#: Go 4) :#: Turn (-20) :#: Go 6
            :#: Turn (-30) :#: Go 8 :#: Turn (-50) :#: Go 4 :#: Turn (-140) :#: Go 9 :#: Turn 70 :#: Go 8 :#: Turn 165 :#: Branch (Turn 180 :#: Go 2 :#: Turn (-180) :#: Go 7 :#: Turn (-30) :#: Go 5 :#: Turn (-40) :#: Go 2) :#:
            Turn (-80) :#: GrabPen white :#: Go 2 :#: GrabPen x :#: Turn 70 :#: Go 5 :#: Turn (-30) :#: Go 7 :#: Turn (-25) :#: Go 9 :#: Turn (-40) :#: Go 12 :#: Turn (-30) :#: Go 2 :#: Branch (Turn (-145) :#: Go 10 :#: Turn 30 :#: Go 9 :#: Turn 30 :#: Go 5 :#: Turn 30 :#: Go 5)
            :#: Go 2 :#: Branch (Turn (-135) :#: Go 12 :#: Turn 30 :#: Go 9 :#: Turn 55 :#: Go 8) :#: Go 5 :#: Branch (Turn (-145) :#: Go 10 :#: Turn 40 :#: Go 10 :#: Turn 40 :#: Go 5) :#: Branch(Go 5) :#: Turn (-125) :#: Go 6 :#: Branch (Go 6 :#: Turn 60 :#: Go 4 :#: Turn (-35) :#: Go 7) :#:
            Turn 135 :#: Go 14 :#: Turn (-40) :#: Go 4 :#: Branch (Turn (-135):#: Go 10 :#: Turn 45 :#: Go 6 :#: Turn 40 :#: Go 3) :#: Go 5 :#: Branch (Turn (-145) :#: Go 8 :#: Turn 40 :#: Go 6) :#: Branch (Turn 20 :#: Go 5) :#: Turn (-125) :#: Go 5 :#: Branch ( Turn 15 :#: Go 5) :#: Turn 160 :#:
            Go 6 :#: Turn (-32) :#: Go 20 :#: Branch ( Turn (-170) :#: Go 18 :#: Turn 40 :#: Go 3) :#: Branch (Turn (-140) :#: Go 5 :#: Turn (-30) :#: Go 12) :#: Turn 15 :#: Go 12 :#: Turn (-45) :#: Go 8 :#: Turn (-40) :#: Go 4 :#: Branch (Turn (-140) :#: Go 7 :#: Turn 40 :#: Go 12 :#: Branch (Turn 30 :#: Go 6 :#: Turn (-30) :#: Go 8) :#: Go 16) :#: Turn 40 :#: Go 6 :#: Turn (-45) :#: Go 6 :#: Turn (-45) :#: Go 5 :#:
            Branch(Turn (-140) :#: Go 8 :#: Turn 50 :#: Go 10 :#: Turn 39.5 :#: Go 14.7 :#: Turn 165 :#: Go 10 :#: Turn (-20) :#: Go 7 :#: GrabPen white :#: Turn (-90) :#: Go 2 :#: Branch (GrabPen x :#: Turn (-90) :#:Go 4 :#:Turn (15) :#: Go 4.8) :#: Turn 110 :#: Go 4 :#: GrabPen x :#: Turn (-150) :#: Go 4 :#: Turn (-40) :#: Go 2.9) :#: Turn (-45) :#: Go 6.7

--drawing multiple copies of einstein
einsteins :: Pen -> Int -> Int -> Command
einsteins x n m = Turn 180 :#: einsteinsHelper x n m

einsteinsHelper :: Pen -> Int -> Int -> Command
einsteinsHelper x 0 m = Go 0
einsteinsHelper x 1 m = (Branch(einsteinCopy x m) :#: Turn (90) :#: GrabPen white :#: Go 25 :#: Turn (-90) :#: Turn (180) :#: Go 120 :#: Turn 90 :#: Go 25 :#: Turn 90)
einsteinsHelper x n m = (Branch(einsteinCopy x m) :#: Turn (90) :#: GrabPen white :#: Go 25 :#: Turn (-90) :#: Turn (180) :#: Go 120 :#: Turn 90 :#: Go 25 :#: Turn 90) :#: einsteinsHelper (next x) (n-1) m

einsteinCopy :: Pen -> Int -> Command
einsteinCopy x 0 = Go 0
einsteinCopy x 1 =  move x
einsteinCopy x n = move x :#: einsteinCopy (next x) (n-1)

move :: Pen -> Command
move x =  GrabPen white :#: Go 2.5 :#: Turn 29 :#: Go 28 :#: Turn 126 :#: Go 75 :#: Turn 33 :#: einstein x :#: Turn (-20) 

-- choosing next colour
toPen :: Int -> Pen
toPen 0 = black
toPen 1 = red
toPen 2 = green
toPen 3 = blue
toPen 4 = yellow

toInt :: Pen -> Int
toInt (Colour 0.0 0.0 0.0) = 0
toInt (Colour 1.0 0.0 0.0) = 1
toInt (Colour 0.0 1.0 0.0) = 2
toInt (Colour 0.0 0.0 1.0) = 3
toInt (Colour 1.0 0.8 0.0) = 4

next :: Pen -> Pen
next x = toPen ((toInt x + 1) `mod` 5)




