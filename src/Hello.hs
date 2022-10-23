module Hello where

import Core
import List
import Nat

encode'' :: Char -> SKI ((a -> a) -> a -> a)
encode'' '\n' = czero'
encode'' n = csucc' :- encode'' (pred n)

hello :: SKI ((((b1 -> b1) -> b1 -> b1) -> b2 -> b2) -> b2 -> b2)
hello =
  listToCh' $
    map encode'' "Hello, World!\n"

-- foo :: IO ()
-- foo =
--   ( listToCh $
--       map encode'' "Hello, World!\n"
--   )
--     (\cur -> (>>) (putChar (cur succ '\n')))
--     (return ())

-- foo :: IO ()
-- foo' =
--   s
--     (s i (k (s (k (>>)) (s (k putChar) (s (s i (k succ)) (k '\n'))))))
--     (k (return ()))
--     ( listToCh $
--         map encode'' "Hello, World!\n"
--     )
