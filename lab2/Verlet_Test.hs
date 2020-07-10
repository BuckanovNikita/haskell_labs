{-# LANGUAGE OverloadedStrings #-} 
import EasyTest
import Verlet

eqPoints :: (Point t) => [t] -> [t] -> Double
eqPoints x y =
    let helper a b = vectorNorm (a .- b)
    in maximum $ zipWith helper x y

allTests = tests [
    scope "1D" $ tests [
        expect $ let
            acceleration :: Double -> Double
            acceleration _ = 0.0
            ans1D = take 4 $ verlet acceleration 0 1 0.1
        in eqPoints ans1D [0.0, 0.1, 0.2, 0.3] < 1e-5
    ]
    ,
    scope "2D" $ tests [
        expect $ let
            acceleration :: (Double, Double) -> (Double, Double)
            acceleration _ = (0.0, 0.0)
            ans2D = take 4 $ verlet acceleration (0,0) (1,1) 0.1
        in eqPoints ans2D [(0.0, 0.0), (0.1, 0.1), (0.2, 0.2), (0.3, 0.3)] < 1e-5
    ]
    ,
    scope "3D" $ tests [
        expect $ let
            acceleration :: [Double] -> [Double]
            acceleration _ = [0.0, 0.0, 0.0]
            ans3D = take 4 $ verlet acceleration [0,0,0] [1, 1, 1] 0.1
        in eqPoints ans3D [[0.0, 0.0, 0.0], [0.1, 0.1, 0.1], [0.2, 0.2, 0.2], [0.3, 0.3, 0.3]] < 1e-5
    ]
 ]

main = EasyTest.run allTests
