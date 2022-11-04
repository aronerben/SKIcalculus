module Hello where

import Core
import List
import Nat

-- TODO encode the chars smarter (32 not naive but as 2^5)
hello :: SKI ((((b1 -> b1) -> b1 -> b1) -> b2 -> b2) -> b2 -> b2)
hello =
  listToCh' $
    map encodeEnum' "Hello, World!\n"

printExcl :: IO ()
printExcl =
  s
    (s i (k (s (k (>>)) (s (k putChar) (s (s i (k succ)) (k (toEnum 0)))))))
    (k (return ()))
    ( s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))))))))))))))))))))))))))))) (k i)
    )

-- foo :: IO ()
-- foo =
--   s
--     (s i (k (s (k (>>)) (s (k putChar) (s (s i (k succ)) (k '\n'))))))
--     (k (return ()))
--     ( listToCh $
--         map encode'' "Hello, World!\n"
--     )
