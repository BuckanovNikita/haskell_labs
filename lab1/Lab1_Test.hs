{-# LANGUAGE OverloadedStrings #-}

module Lab1_Test where

import EasyTest
import Control.Applicative

import Lab1

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
  ]

main = run allTests -- runOnly "xor" allTests
                    -- rerun XXXX (или rerunOnly XXXX "xor") для повтора случайных тестов