{-# LANGUAGE OverloadedStrings #-}

import EasyTest
import SimpleLang

allTests = tests
    [ scope "state" $ tests
        [ expect $ empty "xdf" == 0
        , expect $ (let state1 = extend empty "a" 1 in (state1 "a", state1 "b")) == (1, 0)
        ]
    , scope "eval" $ tests
        [ expect $ eval empty (Val 5) == 5
        , expect $ eval (extend empty "a" 1) (Op (Val 1) Eql (Var "a")) == 1
        ]
    , scope "desugar" $ tests 
        [ expect $ desugar (Incr "A") == DAssign "A" (Op (Var "A") Plus (Val 1))
        ]
    , scope "run" $ tests
        [ expect $ runSimpler empty (DAssign "A" (Val 10)) "A" == 10
        , expect $ SimpleLang.run empty (Incr "A") "A" == 1
        , expect $ (let s = SimpleLang.run (extend empty "In" 4) factorial in s "Out") == 24
        ]
    ]

main = EasyTest.run allTests