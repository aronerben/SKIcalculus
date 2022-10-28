module Bool where

import Core

-- Booleans encoded as Church Booleans
-- https://en.wikipedia.org/wiki/SKI_combinator_calculus#Boolean_logic
true :: a -> b -> a
true = k

false :: a -> b -> b
-- false = s k does not work (can't unify types)
false = k (s k i)

-- Negate
neg :: ((a -> b -> b) -> (c -> d -> c) -> e) -> e
neg = s (s i (k false)) (k true)

-- And
(&.&) :: (a -> (b -> c -> c) -> d) -> a -> d
(&.&) = s s (k (k false))

infixr 3 &.&

-- Or
(|.|) :: ((a -> b -> a) -> c) -> c
(|.|) = s i (k true)

infixr 2 |.|

-- Readable to Church encoded
-- To encode, "c" can be polymorphic since functions represent booleans regardless of type
encode :: c -> (a -> b -> c)
encode bool _ _ = bool

-- Specifying "a" and "b" to Bool when interpreting into readable data
decode :: (Bool -> Bool -> a) -> a
decode bski = bski True False

-- False || True = True
example1 :: Bool
example1 = decode $ false |.| true

-- False && True = False
example2 :: Bool
example2 = decode $ false &.& true

-- !False && True = True
example3 :: Bool
example3 = decode $ neg false &.& encode True

-- (False || !True) && True = False
example4 :: Bool
example4 = decode $ false |.| neg true &.& true

-- With show instance
true' :: SKI (a -> b -> a)
true' = K

false' :: SKI (a -> b -> b)
false' = K :- (S :- K :- I)

-- Negate
neg' :: SKI (((a -> b -> b) -> (c -> d -> c) -> e) -> e)
neg' = S :- (S :- I :- (K :- false')) :- (K :- true')

-- Can't be infixes due to constructor :-
and' :: SKI ((a -> (b -> c -> c) -> d) -> a -> d)
and' = S :- S :- (K :- (K :- false'))

or' :: SKI (((a -> b -> a) -> c) -> c)
or' = S :- I :- (K :- true')

-- (False || !True) && True = False
-- Interesting: example4' has truth value encoded in type!
example4' :: SKI (b -> c -> c)
example4' = and' :- (or' :- false' :- (neg' :- true')) :- true'

-- example4' in GHCi prints:
{-
((((s s) (k (k (k ((s k) i))))) ((((s i) (k k)) (k ((s k) i))) (((s ((s i) (k (k ((s k) i))))) (k k)) k))) k)
-}
-- Using Show instance of SKI GADT

-- Can now use the printed result for pure SKI version
example4'' :: Bool
example4'' = decode $ s s (k (k (k (s k i)))) (s i (k k) (k (s k i)) (s (s i (k (k (s k i)))) (k k) k)) k
