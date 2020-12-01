module UIME where

import LSystem

uInMyEye :: Int -> Command
uInMyEye x = f x
    where
        f 0 = Go 10
        f x = p :#: f(x-1) 
                :#: p :#: f(x-1) 
                :#: p :#: f(x-1) 
                :#: p :#: f(x-1)
        p = Turn (-15)