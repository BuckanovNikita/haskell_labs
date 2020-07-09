{-# LANGUAGE OverloadedStrings #-}

module Lab1_Test where

import EasyTest
import Control.Applicative

import Lab1
import Luhn

-- Для запуска сделайте двойной щелчок по этому файлу или зайдите в директорию и запустите 
-- ghci Lab1_Test из командной строки.
-- Для перезагрузки после изменений (в Lab1 или Lab1_Test) используйте команду :reload (сокращённо :r) внутри GHCi.
-- Не забудьте попробовать :help (:h)!

-- документация EasyTest в EasyTest.html (http://hackage.haskell.org/package/easytest-0.2.1/docs/EasyTest.html)
-- там используются понятия и синтаксис, которые нам пока незнакомы, не беспокойтесь об этом
-- к концу курса вы должны быть способны понять большую часть EasyTest :)
-- <|> комбинирует тесты

-- обратите внимание на оформление списка: так удобнее добавлять, удалять и комментировать элементы в конце
allTests = tests
  -- scope даёт название тесту (и может быть вложено)
  [ scope "xor" $ tests 
      -- expect проверяет условие (безусловный успех: ok, безусловная ошибка: crash "причина")
      [ expect $ xor True True == False
      , expect $ xor True False == True
      , expect $ xor False True == True
      , expect $ xor False False == False
      ]
    , scope "max3" $ -- или tests [ expect (max3 1 3 2 == 3), expect (max3 5 2 5 == 5) ]
      expect (max3 1 3 2 == 3) <|>
      expect (max3 5 2 5 == 5)
    -- добавьте тесты для остальных функций!
    , scope "geomprogression" $ tests
        [
         expect $ geomProgression 1 1 0 == 1.0
        ,expect $ geomProgression 0 0 0 == 0.0
        ,expect $ geomProgression 4 2 2 == 16.0
        ,expect $ geomProgression 2 2 10 == 2048.0
        ]
     , scope "coprime" $ tests
        [
           expect $ coprime 0 5 == False
           ,expect $ coprime 4 9 == True
           ,expect $ coprime 1 100 == True
           ,expect $ coprime 0 0 == False
           ,expect $ coprime 1 1 == True
           ,expect $ coprime 32 4096 == False
        ]
      , scope "distance" $ tests
      [
         expect $ distance (Point [1.0, 0, 0, 0, 0]) (Point [0.0, 1.0, 0, 0, 0]) == sqrt 2
        ,expect $ distance (Point [0.0, 1.0, 0, 0, 0]) (Point [0, 1.0, 0, 0, 0]) == 0
      ]
      , scope "intersect" $ tests
      [
         expect $ (intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4]) || (intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] ==  [4, 2])
        ,expect $ intersect [1, 2, 4, 6] [3, 5, 7] == []
      ]
      , scope "zipN" $ tests
      [
          expect $ zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
      ]
      , scope "find" $ tests
      [
          expect $ find (> 0) [-1, 2, -3, 4] == Just 2
         ,expect $ find (> 0) [-1, -2, -3] == Nothing
      ]
      , scope "findLast" $ tests
      [
          expect $ findLast (> 0) [-1, 2, -3, 4] == Just 4
         ,expect $ findLast (> 0) [-1, 2, -3, 4] == Just 4
         ,expect $ findLast (> 0) [-1, -2, -3] == Nothing
      ]
      , scope "mapFuncs" $ tests
      [
          expect $ mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]
      ]
      , scope "satisfiesAll" $ tests
      [
          expect $ satisfiesAll [even,\x -> x `mod` 5 == 0] 10 == True
         ,expect $ satisfiesAll [even,\x -> x `mod` 5 == 0] 11 == False
         ,expect $ satisfiesAll [] 4 == True
      ]
      , scope "Luhn" $ tests
      [
          expect $ isLuhnValid [3,3,3] == False
         ,expect $ isLuhnValid [3,8,0] == True
         ,expect $ isLuhnValid [0,0,0] == True
         ,expect $ isLuhnValid [3,3,1] == True
         ,expect $ isLuhnValid [1,0,0,0,0,0,0,0,0] == False
      ]
      
  ]

main = run allTests -- runOnly "xor" allTests
                    -- rerun XXXX (или rerunOnly XXXX "xor") для повтора случайных тестов