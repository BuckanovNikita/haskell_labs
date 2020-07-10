{-# LANGUAGE OverloadedStrings #-}
module Lab2_Test where
import EasyTest

import SimpleLang
import Poly

allTests = tests [
    scope "applyPoly" $ tests [
        expect $ applyPoly (P [1, 2, 3]) 4 == 57
       ,expect $  applyPoly (P [1, 2, 3]) 0 == 1
       ,expect $  applyPoly (P [1, 2, 3, 0, 0]) 4 == 57
       ,expect $  applyPoly (P []) 222 == 0
       ,expect $  applyPoly (P [0, 0, 0]) 222 == 0
       ,expect $  applyPoly (P [5]) 222 == 5
    ]
    , scope "PolyEqual" $ tests [
        expect $ P [1, 1, 1] == P [1, 1, 1]
       ,expect $ P [1, 1, 2] == P [1, 1, 2, 0, 0]
       ,expect $ P [1, 1, 3, 0, 0] == P [1, 1, 3]
       ,expect $ P [1, 2, 3, 0, 0] /= P [1, 2, 3, 0, 0, 1]
       ,expect $ P [] == P [0, 0, 0]
       ,expect $ P [0, 0] == P []
    ]
    , scope "showPoly" $ tests [
        expect $ show (P [1, 2, 3, -1, -2, -3, 1, 2, 3, -1, -2, 0, 1024]) == "1024 * x^12 - 2 * x^10 - x^9 + 3 * x^8 + 2 * x^7 + x^6 - 3 * x^5 - 2 * x^4 - x^3 + 3 * x^2 + 2 * x + 1"
        ,expect $ show (P []) == "0"
        ,expect $ show (P [1]) == "1"
        ,expect $ show (P [-1.0]) == "-1.0"
        ,expect $ show (P [0.0, 0.0, 0.0, 0.0]) == "0"
        ,expect $ show (P [-1, 2]) == "2 * x - 1"
        ,expect $ show (P [-1, -2, 0]) == "-2 * x - 1"
        ,expect $ show (P [1, 1, -1]) == "-x^2 + x + 1"
    ]
    , scope "plus" $ tests [
        expect $ plus (P [0, 0, 0]) (P [0, 0]) == P []
       ,expect $ plus (P [0, 1]) (P [1, 2, 3]) == P [1, 3, 3]
       ,expect $ plus (P [0, 1, 0]) (P [0, -1]) == P []
       ,expect $ plus (P [0, 0, 0]) (P [0, -1]) == P [0, -1]
    ]
    , scope "times" $ tests [
        expect $ times (P [3, 1, 2]) (P [7, 5]) == P [21, 22, 19, 10]
       ,expect $ times (P []) (P [1, 2]) == P []
       ,expect $ times (P [1, 2]) (P []) == P []
       ,expect $ times (P [1, 2, 0, 1]) (P [0, 2, 0]) == P [0, 2, 4, 0, 2]
    ]
    , scope "negate" $ tests [
        expect $ negate (P [-1, 2, -3]) == P [1, -2, 3]
    ]
    , scope "fromInteger" $ tests [
       expect $ P [3.5] + 4 == P [7.5]
      ,expect $ P [3] + 4.5 == P [7.5]
    ]
    , scope "deriv" $ tests [
        expect $ deriv (P [1]) == P []
       ,expect $ deriv (P []) == P []
       ,expect $ deriv (P [10, 3, 0, 5]) == P [3, 0, 15]
    ]
    , scope "nderiv" $ tests [
        expect $ nderiv 0 (P [1, 2, 3]) == P [1, 2, 3]
       ,expect $ nderiv 1 (P [1, 2, 3]) == P [2, 6]
       ,expect $ nderiv 2 (P [1, 2, 3]) == P [6]
       ,expect $ nderiv 3 (P [1, 2, 3]) == P []
    ]
    , scope "state" $ tests [
        expect $ empty "xdf" == 0
       ,expect $ (let state1 = extend empty "a" 1 in (state1 "a", state1 "b")) == (1, 0)
    ]
    , scope "eval" $ tests [
        expect $eval empty (Val 5) == 5
       ,expect $ eval (extend empty "a" 5) (Op (Val 2) Plus (Var "a")) == 7
       ,expect $ eval (extend empty "a" 5) (Op (Val 2) Minus (Var "a")) == -3
       ,expect $ eval (extend empty "a" 5) (Op (Val 2) Times (Var "a")) == 10
       ,expect $ eval (extend empty "a" 5) (Op (Val 12) Divide (Var "a")) == 2
       ,expect $ eval (extend empty "a" 5) (Op (Val 5) Gt (Var "a")) == 0
       ,expect $ eval (extend empty "a" 5) (Op (Val 5) Ge (Var "a")) == 1
       ,expect $ eval (extend empty "a" 5) (Op (Val 12) Lt (Var "a")) == 0
       ,expect $ eval (extend empty "a" 5) (Op (Val 5) Le (Var "a")) == 1
       ,expect $ eval (extend empty "a" 1) (Op (Val 1) Eql (Var "a")) == 1
    ]
    ,scope "desugar" $ tests [
       expect $  desugar (Incr "A") == DAssign "A" (Op (Var "A") Plus (Val 1))
    ]
    ,scope "run" $ tests [
        expect $ runSimpler empty (DAssign "A" (Val 10)) "A" == 10
       ,expect $ SimpleLang.run (extend empty "A" 4) (Skip) "A" == 4
       ,expect $ SimpleLang.run empty (Incr "A") "A" == 1
       ,expect $ SimpleLang.run empty (Block [
                                       Incr "A"
                                       , Incr "A"
                                      ]
                                ) "A" == 2

        ,expect $ SimpleLang.run (extend empty "A" 4) (If (Op (Var "A") Ge (Val 10))
                                                  (Assign "D" (Val 50))
                                                  (Assign "D" (Val 200))
                                               ) "D" == 200

        ,expect $ SimpleLang.run (extend empty "A" 5) (While (Op (Var "A") Le (Val 12))
                                                     (Assign "A" (Op (Var "A") Plus (Val 3)))
                                               ) "A" == 14

        ,expect $ SimpleLang.run empty (For (Assign "A" (Val 5))
                                    (Op (Var "A") Le (Val 12))
                                    (Assign "A" (Op (Var "A") Plus (Val 3)))
                                    Skip
                                ) "A" == 14

        ,expect $  (let s = SimpleLang.run (extend empty "In" 4) factorial in s "Out") == 24
    ]
    ,scope "factorial" $ tests [
        expect $ (SimpleLang.run (extend empty "In" 1) factorial) "Out" == 1
       ,expect $  (SimpleLang.run (extend empty "In" 2) factorial) "Out" == 2
       ,expect $  (SimpleLang.run (extend empty "In" 3) factorial) "Out" == 6
       ,expect $  (SimpleLang.run (extend empty "In" 5) factorial) "Out" == 120
    ]
    , scope "squareRoot" $ tests [
        expect $ (SimpleLang.run (extend empty "A" 37) squareRoot) "B" == 6
       ,expect $  (SimpleLang.run (extend empty "A" 36) squareRoot) "B" == 6
    ]
    , scope "fibonacci" $ tests [
        expect $  (SimpleLang.run (extend empty "In" 0) fibonacci) "Out" == 1
       ,expect $  (SimpleLang.run (extend empty "In" 1) fibonacci) "Out" == 1
       ,expect $  (SimpleLang.run (extend empty "In" 2) fibonacci) "Out" == 2
       ,expect $  (SimpleLang.run (extend empty "In" 3) fibonacci) "Out" == 3
       ,expect $  (SimpleLang.run (extend empty "In" 4) fibonacci) "Out" == 5
       ,expect $  (SimpleLang.run (extend empty "In" 5) fibonacci) "Out" == 8
    ]
 ]

main = EasyTest.run allTests
