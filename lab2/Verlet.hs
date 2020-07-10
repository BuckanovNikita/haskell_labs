{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Verlet where

class Point t where
    infixl 6 .+
    (.+) :: t -> t -> t
    
    infixl 6 .-
    (.-) :: t -> t -> t
    (.-) x y = x .+ ((-1.0) .* y)
    
    infixl 7 .*
    (.*) :: Double -> t -> t
    
    
    vectorNorm :: t -> Double

instance Point Double where
    x .+ y = x + y
    x .* y = x * y
    vectorNorm = abs

instance Point (Double, Double) where
    (x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)
    a .* (x, y) = (a * x, a * y)
    vectorNorm (x, y) = max (abs x) (abs y)

instance Point [Double] where
    xs .+ ys = zipWith (+) xs ys
    x .* ys = map (*x) ys
    vectorNorm xs = maximum $ map abs xs

verlet :: Point t => (t -> t) -> t -> t -> Double -> [t]
verlet f x0 v0 dt =
    let x1 = x0 .+ dt .* v0 .+ (0.5 * dt * dt) .*(f x0)
        verlet' xPrev xCurr =
                let xNext = 2.0 .* xCurr .- xPrev .+ (dt * dt) .* (f xCurr)
                in  xCurr : verlet' xCurr xNext
    in  x0 : verlet' x0 x1