module Nat where

import Core

-- Naturals encoded as Church Numerals
-- https://en.wikipedia.org/wiki/Church_encoding#Church_numerals

type ChurchNumeral a = (a -> a) -> a -> a

-- Zero
-- \f -> \x -> x

-- Use https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis to transform to SKI
-- T[\f -> \x -> x]
-- K T[\x -> x]      by R3
-- K I               by R4
czero :: a -> b -> b
czero = k i

-- Succ
-- Successor is built by composing functions
-- Zero := \f -> \x -> x
-- One  := \f -> \x -> f x
-- Two  := \f -> \x -> f (f x)
-- Succ := \(n : ChurchNumeral a) -> (\f -> \x -> f (n f x))
-- Use transformer
csucc :: ChurchNumeral a -> ChurchNumeral a
csucc = s (s (k s) k)

-- Readable to Church encoded
encode :: Int -> ChurchNumeral a
encode 0 = czero
encode n = csucc $ encode $ pred n

decode :: ChurchNumeral Int -> Int
decode fn = fn succ 0

three :: Int
three = decode $ csucc $ csucc $ csucc czero

fiftyfive :: Int
fiftyfive = decode $ encode 55

-- To encode, "a" can be polymorphic since functions represent enumerables regardless of type
encodeEnum :: Enum a => a -> ChurchNumeral a
encodeEnum n | fromEnum n == 0 = czero
encodeEnum n = csucc $ encodeEnum $ pred n

decodeEnum :: Enum a => ChurchNumeral a -> a
decodeEnum fn = fn succ (toEnum 0)

seven :: Int
seven = decodeEnum $ encodeEnum 7

newline :: Char
newline = decodeEnum $ encodeEnum '\n'

-- Add
(@+) :: (a -> b -> c) -> (a -> d -> b) -> a -> d -> c
(@+) = s (k s) k s (s (k s) k (s (k s) k))

infixl 6 @+

-- Mul
(@*) :: (b -> c) -> (a -> b) -> a -> c
(@*) = s (k s) k

infixl 7 @*

-- Exp
(@^) :: b -> (b -> c) -> c
(@^) = s (s (k (s (k s) k)) s) (k k) i

infixr 8 @^

-- Halving
-- https://codegolf.stackexchange.com/questions/198840/ski-calculus-golf-half-of-a-church-numeral
halve ::
  ( ( ((((b1 -> c1) -> a1 -> b1) -> a2 -> c2) -> c3) ->
      (a2 -> ((b1 -> c1) -> a1 -> c1) -> c2) ->
      c3
    ) ->
    (((b2 -> a3 -> a3) -> (b3 -> a4 -> a4) -> c4) -> c4) ->
    (a5 -> b4 -> a5) ->
    c5
  ) ->
  c5
halve = s (s (s i (k (s (s (k s) k) (k (s (s (k s) (s (k k) s)) (k (s (k k) (s (s (k s) k))))))))) (k (s (s i (k (k i))) (k (k i))))) (k k)

example :: Int
example = decode $ halve $ (e 2 @* e 2) @^ (e 3 @+ e 5)
  where
    e = encode

-- With show instance
czero' :: SKI (a -> b -> b)
czero' = K :- I

csucc' :: SKI (((b -> c) -> a -> b) -> (b -> c) -> a -> c)
csucc' = S :- (S :- (K :- S) :- K)

encodeEnum' :: Enum a => a -> SKI (ChurchNumeral b)
encodeEnum' n | fromEnum n == 0 = czero'
encodeEnum' n = csucc' :- encodeEnum' (pred n)

fiftyfive' :: SKI (ChurchNumeral a)
fiftyfive' = encodeEnum' (55 :: Int)

-- fiftyfive' in GHCi prints:
{-
((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) (k i))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-}
-- Using Show instance of SKI GADT

-- Can now use the printed result for pure SKI version
fiftyfive'' :: Int
fiftyfive'' = decode $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))))))))))))))))))))))))))))))))))))))))))))))))))

seven' :: SKI (ChurchNumeral a)
seven' = encodeEnum' (7 :: Int)

-- Following printed results from GHCi for pure SKI version
seven'' :: Int
seven'' = decodeEnum $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))

newline' :: SKI (ChurchNumeral a)
newline' = encodeEnum' '\n'

newline'' :: Char
newline'' = decodeEnum $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))))))))

add' :: SKI ((a -> b -> c) -> (a -> d -> b) -> a -> d -> c)
add' = S :- (K :- S) :- K :- S :- (S :- (K :- S) :- K :- (S :- (K :- S) :- K))

mul' :: SKI ((b -> c) -> (a -> b) -> a -> c)
mul' = S :- (K :- S) :- K

exp' :: SKI (b -> (b -> c) -> c)
exp' = S :- (S :- (K :- (S :- (K :- S) :- K)) :- S) :- (K :- K) :- I

halve' ::
  SKI
    ( ( ( ((((b1 -> c1) -> a1 -> b1) -> a2 -> c2) -> c3) ->
          (a2 -> ((b1 -> c1) -> a1 -> c1) -> c2) ->
          c3
        ) ->
        (((b2 -> a3 -> a3) -> (b3 -> a4 -> a4) -> c4) -> c4) ->
        (a5 -> b4 -> a5) ->
        c5
      ) ->
      c5
    )
halve' = S :- (S :- (S :- I :- (K :- (S :- (S :- (K :- S) :- K) :- (K :- (S :- (S :- (K :- S) :- (S :- (K :- K) :- S)) :- (K :- (S :- (K :- K) :- (S :- (S :- (K :- S) :- K))))))))) :- (K :- (S :- (S :- I :- (K :- (K :- I))) :- (K :- (K :- I))))) :- (K :- K)

example' :: SKI (ChurchNumeral Int)
example' = halve' :- exp' :- (mul' :- e (2 :: Int) :- e 2) :- (add' :- e 3 :- e 5)
  where
    e = encodeEnum'

-- From GHCi running example'
example'' :: Int
example'' = decode $ s (s (s i (k (s (s (k s) k) (k (s (s (k s) (s (k k) s)) (k (s (k k) (s (s (k s) k))))))))) (k (s (s i (k (k i))) (k (k i))))) (k k) (s (s (k (s (k s) k)) s) (k k) i) (s (k s) k (s (s (k s) k) (s (s (k s) k) (k i))) (s (s (k s) k) (s (s (k s) k) (k i)))) (s (k s) k s (s (k s) k (s (k s) k)) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))

-- Represent 55 as 5 * 11 to shorten original
-- mul' :- (e 5) :- (e 11)
fiftyfive''' :: Int
fiftyfive''' = decode $ s (k s) k (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))))))))))
