module Nat where

import Core

-- Naturals encoded as Church Numerals
-- https://en.wikipedia.org/wiki/Church_encoding#Church_numerals

type ChurchNumeral a = (a -> a) -> a -> a

-- Can't use this, as SKI (ChurchNumeral) would need impredicative polymorphism
-- type ChurchNumeral = forall a. (a -> a) -> a -> a

-- Zero
-- \f -> \x -> x

-- Use https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis to transform to SKI
-- T[\f -> \x -> x]
-- K T[\x -> x]      by R3
-- K I               by R4
czero :: ChurchNumeral a
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
encode :: Integer -> ChurchNumeral a
encode 0 = czero
encode n = csucc $ encode $ pred n

decode :: ChurchNumeral Integer -> Integer
decode fn = fn succ 0

three :: Integer
three = decode $ csucc $ csucc $ csucc czero

fiftyfive :: Integer
fiftyfive = decode $ encode 55

-- To encode, "a" can be polymorphic since functions represent enumerables regardless of type
encodeEnum :: Enum a => a -> ChurchNumeral b
encodeEnum n | fromEnum n == 0 = czero
encodeEnum n = csucc $ encodeEnum $ pred n

decodeEnum :: Enum a => ChurchNumeral a -> a
decodeEnum fn = fn succ (toEnum 0)

-- transform :: ChurchNumeralhar -> ChurchNumeralnt
-- transform nr fn start = _

seven :: Integer
seven = decodeEnum $ encodeEnum (7 :: Integer)

newline :: Char
newline = decodeEnum $ encodeEnum '\n'

-- Add
(@+) :: ChurchNumeral a -> ChurchNumeral a -> ChurchNumeral a
(@+) = s (k s) (s (k (s (k s) k)))

infixl 6 @+

-- Mul
(@*) :: ChurchNumeral a -> ChurchNumeral a -> ChurchNumeral a
(@*) = s (k s) k

infixl 7 @*

-- Exp
(@^) :: ChurchNumeral a -> ChurchNumeral (a -> a) -> ChurchNumeral a
-- (@^) a b = instN $ (s (k (s i)) k) (N a) (N b)
-- TODO this just applies churchnumeral to itself...
(@^) = s (k (s i)) k

-- (@^) = undefined

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

example :: Integer
example = decode $ halve $ (e 2 @* e 2) @^ (e 3 @+ e 5)
  where
    e = encode

-- With show instance

type ChurchNumeral' a = (a -> a) -> a -> a

czero' :: SKI (ChurchNumeral' a)
czero' = K :- I

csucc' :: SKI (ChurchNumeral' a -> ChurchNumeral' a)
csucc' = S :- (S :- (K :- S) :- K)

encodeEnum' :: Enum a => a -> SKI (ChurchNumeral' b)
encodeEnum' n | fromEnum n == 0 = czero'
encodeEnum' n = csucc' :- encodeEnum' (pred n)

fiftyfive' :: SKI (ChurchNumeral' a)
fiftyfive' = encodeEnum' (55 :: Integer)

-- fiftyfive' in GHCi prints:
{-
((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) (k i))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-}
-- Using Show instance of SKI GADT

-- Can now use the printed result for pure SKI version
fiftyfive'' :: Integer
fiftyfive'' = decode $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))))))))))))))))))))))))))))))))))))))))))))))))))

seven' :: SKI (ChurchNumeral' a)
seven' = encodeEnum' (7 :: Int)

-- Following printed results from GHCi for pure SKI version
seven'' :: Int
seven'' = decodeEnum $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))

newline' :: SKI (ChurchNumeral' a)
newline' = encodeEnum' '\n'

newline'' :: Char
newline'' = decodeEnum $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))))))))

add' :: SKI (ChurchNumeral' a -> ChurchNumeral' a -> ChurchNumeral' a)
add' = S :- (K :- S) :- (S :- (K :- (S :- (K :- S) :- K)))

mul' :: SKI (ChurchNumeral' a -> ChurchNumeral' a -> ChurchNumeral' a)
mul' = S :- (K :- S) :- K

-- exp' :: SKI (ChurchNumeral' a -> ChurchNumeral' a -> ChurchNumeral' a)
-- TODO fix this
exp' :: SKI (b -> (b -> c) -> c)
exp' = S :- (K :- (S :- I)) :- K

-- exp' = undefined

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

example' :: SKI (ChurchNumeral' Int)
example' = halve' :- (exp' :- (mul' :- e (2 :: Int) :- e 2) :- (add' :- e 3 :- e 5))
  where
    e = encodeEnum'

-- From GHCi running example'
example'' :: Integer
example'' = decode $ s (s (s i (k (s (s (k s) k) (k (s (s (k s) (s (k k) s)) (k (s (k k) (s (s (k s) k))))))))) (k (s (s i (k (k i))) (k (k i))))) (k k) (s (s (k (s (k s) k)) s) (k k) i) (s (k s) k (s (s (k s) k) (s (s (k s) k) (k i))) (s (s (k s) k) (s (s (k s) k) (k i)))) (s (k s) k s (s (k s) k (s (k s) k)) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))

-- Represent 55 as 3^3 * 2 + 1 to shorten original
-- csucc' (mul' :- (exp' :- (e 3) :- (e 3)) :- (e 2))
fiftyfive''' :: Integer
fiftyfive''' = decode $ s (s (k s) k) (s (k s) k (s (s (k (s (k s) k)) s) (k k) i (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (k i))))
