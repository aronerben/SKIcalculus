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

-- T[\n -> \f -> \x -> f (n f x)]
-- T[\n -> T[\f -> \x -> f (n f x)]] by R5
-- T[\n -> T[\f -> T[\x -> f (n f x)]]] by R5
-- T[\n -> T[\f -> (S T[\x -> f] T[\x -> (n f x)])]] by R6
-- T[\n -> T[\f -> (S (K f) T[\x -> (n f x)])]] by R31
-- T[\n -> T[\f -> (S (K f) (S T[\x -> n f] T[\x -> x]))]] by R6
-- T[\n -> T[\f -> (S (K f) (S (K (n f)) T[\x -> x]))]] by R3211
-- T[\n -> T[\f -> (S (K f) (S (K (n f)) I))]] by R4
-- T[\n -> (S T[\f -> S (K f)] T[\f -> (S (K (n f)) I)])] by R6
-- T[\n -> (S (S T[\f -> S] T[\f -> (K f)]) T[\f -> (S (K (n f)) I)])] by R6
-- T[\n -> (S (S (K S) T[\f -> (K f)]) T[\f -> (S (K (n f)) I)])] by R31
-- T[\n -> (S (S (K S) (S T[\f -> K] T[\f -> f])) T[\f -> (S (K (n f)) I)])] by R6
-- T[\n -> (S (S (K S) (S (K K) T[\f -> f])) T[\f -> (S (K (n f)) I)])] by R31
-- T[\n -> (S (S (K S) (S (K K) I)) T[\f -> (S (K (n f)) I)])] by R4
-- T[\n -> (S (S (K S) (S (K K) I)) (S T[\f -> S (K (n f))] T[\f -> I]))] by R6
-- T[\n -> (S (S (K S) (S (K K) I)) (S T[\f -> S (K (n f))] (K I)))] by R31
-- T[\n -> (S (S (K S) (S (K K) I)) (S (S T[\f -> S] T[\f -> (K (n f))]) (K I)))] by R6
-- T[\n -> (S (S (K S) (S (K K) I)) (S (S (K S) T[\f -> (K (n f))]) (K I)))] by R31
-- T[\n -> (S (S (K S) (S (K K) I)) (S (S (K S) (S T[\f -> K] T[\f -> (n f)])) (K I)))] by R6
-- T[\n -> (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) T[\f -> (n f)])) (K I)))] by R31
-- T[\n -> (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) (S T[\f -> n] T[\f -> f]))) (K I)))] by R6
-- T[\n -> (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) (S (K n) T[\f -> f]))) (K I)))] by R31
-- T[\n -> (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) (S (K n) I))) (K I)))] by R4

-- Alternatively,
-- Succ := \(n : ChurchNumeral a) -> (\f -> f . (n f))
-- Write compose as prefix for simplicity
-- T[\n -> \f -> (.) f (n f)]
-- T[\n -> T[\f -> (.) f (n f)]] by R5
-- T[\n -> (S T[\f -> (.) f] T[\f -> (n f)])] by R6
-- T[\n -> (S T[\f -> (.) f] (S T[\f -> n] T[\f -> f]))] by R6
-- T[\n -> (S T[\f -> (.) f] (S T[\f -> n] I))] by R4
-- T[\n -> (S T[\f -> (.) f] (S (K n) I))] by R31
-- T[\n -> (S (S T[\f -> (.)] T[\f -> f]) (S (K n) I))] by R6
-- T[\n -> (S (S (K (.)) T[\f -> f]) (S (K n) I))] by R31
-- T[\n -> (S (S (K (.)) I) (S (K n) I))] by R4
-- S T[\n -> S (S (K (.)) I)] T[\n -> (S (K n) I)] by R6
-- S (K (S (S (K (.)) I))) T[\n -> (S (K n) I)] by R31
-- S (K (S (S (K (.)) I))) (S T[\n -> S (K n)] T[\n -> I]) by R6
-- S (K (S (S (K (.)) I))) (S T[\n -> S (K n)] (K I)) by R31
-- S (K (S (S (K (.)) I))) (S (S T[\n -> S] T[\n -> (K n)]) (K I)) by R6
-- S (K (S (S (K (.)) I))) (S (S (K S) (S T[\n -> K] T[\n -> n])) (K I)) by R6
-- S (K (S (S (K (.)) I))) (S (S (K S) (S (K K) T[\n -> n])) (K I)) by R31
-- S (K (S (S (K (.)) I))) (S (S (K S) (S (K K) I)) (K I)) by R31

-- T[\n -> (S T[\f -> (.) f] T[\f -> (n f)])] by R6
-- T[\n -> (S (.) n)] by eta reduction
-- T[S (.)] by eta reduction

foo = s (k (s (s (k (s (k s) k)) i))) (s (s (k s) (s (k k) i)) (k i))

-- foo :- = :- S :- (K :- (S :- (S :- (K :- (S :- (K :- S) :- K)) :- I))) :- (S :- (S :- (K :- S) :- (S :- (K :- K) :- I)) :- (K :- I))

csucc :: ChurchNumeral a -> ChurchNumeral a
csucc = s (s (k s) k)

-- Readable to Church encoded
-- To encode, "a" can be polymorphic since functions represent numerals regardless of type
encode :: Int -> ChurchNumeral a
encode 0 = czero
encode n = csucc $ encode $ n - 1

-- Specifying "a" to Int when interpreting into readable data
decode :: ChurchNumeral Int -> Int
decode fn = fn succ 0

three :: Int
three = decode $ csucc $ csucc $ csucc czero

fiftyfive :: Int
fiftyfive = decode $ encode 55

-- With show instance

czero' :: SKI (a -> b -> b)
czero' = K :- I

csucc' :: SKI (((b -> c) -> a -> b) -> (b -> c) -> a -> c)
csucc' = S :- (S :- (K :- S) :- K)

encode' :: Int -> SKI ((a -> a) -> a -> a)
encode' 0 = czero'
encode' n = csucc' :- encode' (n - 1)

fiftyfive' :: SKI ((a -> a) -> a -> a)
fiftyfive' = encode' 55

-- fiftyfive' in GHCi prints:
{-
((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) (k i))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-}
-- Using Show instance of SKI GADT

-- Can now use the printed result for pure SKI version
fiftyfive'' :: Int
fiftyfive'' = decode $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- -- Church numeral operations
-- cadd :: (a -> b -> c) -> (a -> d -> b) -> a -> d -> c
-- cadd = s (k s) k s (s (k s) k (s (k s) k))

-- cmul :: (b -> c) -> (a -> b) -> a -> c
-- cmul = s (k s) k

-- cexp :: b -> (b -> c) -> c
-- cexp = s (s (k (s (k s) k)) s) (k k) i

-- -- https://codegolf.stackexchange.com/questions/198840/ski-calculus-golf-half-of-a-church-numeral
-- chalf = s (s (s i (k (s (s (k s) k) (k (s (s (k s) (s (k k) s)) (k (s (k k) (s (s (k s) k))))))))) (k (s (s i (k (k i))) (k (k i))))) (k k)

-- e = encode

-- (@+) = cadd

-- (@*) = cmul

-- (@^) = cexp

-- -- (2*2)^(3+5) * 0.5 = 4^8 * 0.5 = 32768
-- example :: Int
-- example = decode $ chalf $ (e 2 @* e 2) @^ (e 3 @+ e 5)
