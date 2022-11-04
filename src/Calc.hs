{-# LANGUAGE GADTs #-}

module Calc where

import Data.Char

-- SKI combinators
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

-- Can be constructed from s and k, not a primitive
i :: a -> a
i = s k k

-- Some constructs
-- true
t :: a -> b -> a
t = k

-- false
f :: a -> b -> b
f x = s k (k i x)

decodeBool :: (Bool -> Bool -> Bool) -> Bool
decodeBool fn = fn True False

-- negate
n :: ((a -> b -> b) -> (c -> d -> c) -> e) -> e
n = s (s i (k f)) (k t)

-- and
a :: (a -> (b -> c -> c) -> d) -> a -> d
a = s s (k (k f))

-- or
o :: ((a -> b -> a) -> c) -> c
o = s i (k t)

-- Church numerals
-- Encoded as functions
czero :: a -> b -> b
czero = k i

-- We want to get f (f (...f x)) which is just f . f ... . f x
-- Thus we need function composition and repetition of function
-- (s (k s) k f1) f2 x is function composition
-- (s (k s) k f1) f2 x =
-- ((k s f1) (k f1)) f2 x =
-- (s (k f1)) f2 x =
-- s (k f1) f2 x =
-- (k f1 x) (f2 x) =
-- f1 (f2 x) =
-- is composition (f1 . f2) x

-- s is our applicative <*>
-- (s compose fstack f) nr =
-- (compose f (fstack f)) nr =
-- (f . (fstack f)) nr =
-- fstack is the function composition stack which collapses after s combinator is applied
-- (f . (f . f ... f)) nr =
csucc :: ((b -> c) -> a -> b) -> (b -> c) -> a -> c
csucc = s (s (k s) k)

encode :: Int -> (a -> a) -> a -> a
encode 0 = czero
encode n = csucc $ encode (n - 1)

decode :: ((Int -> Int) -> Int -> a) -> a
decode fn = fn succ 0

-- Encoded as value constructors
data SKI e where
  S :: SKI ((a -> b -> c) -> (a -> b) -> a -> c)
  K :: SKI (a -> b -> a)
  I :: SKI (a -> a)
  (:-) :: SKI (a -> b) -> SKI a -> SKI b

-- Show instance to transform to function encoding
instance Show (SKI e) where
  show S = "s"
  show K = "k"
  show I = "i"
  show (a :- b) = "(" ++ show a ++ " " ++ show b ++ ")"

czero' = K :- I

csucc' = S :- (S :- (K :- S) :- K)

encode' 0 = czero'
encode' n = csucc' :- encode' (pred n)

-- Use show instance
fiftyfive :: (b -> b) -> b -> b
fiftyfive = s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- Church numeral operations
cadd :: (a -> b -> c) -> (a -> d -> b) -> a -> d -> c
cadd = s (k s) k s (s (k s) k (s (k s) k))

cmul :: (b -> c) -> (a -> b) -> a -> c
cmul = s (k s) k

cexp :: b -> (b -> c) -> c
cexp = s (s (k (s (k s) k)) s) (k k) i

-- https://codegolf.stackexchange.com/questions/198840/ski-calculus-golf-half-of-a-church-numeral
chalf = s (s (s i (k (s (s (k s) k) (k (s (s (k s) (s (k k) s)) (k (s (k k) (s (s (k s) k))))))))) (k (s (s i (k (k i))) (k (k i))))) (k k)

e = encode

(@+) = cadd

(@*) = cmul

(@^) = cexp

-- (2*2)^(3+5) * 0.5 = 4^8 * 0.5 = 32768
example :: Int
example = decode $ chalf $ (e 2 @* e 2) @^ (e 3 @+ e 5)

-- For more functions, write them in lambda cal and mechanically transform:
-- https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis

-- Pairs and lists
pair x y z = z x y

first p = p (const)

second p = p (\x y -> y)

(.:) = pair

infixr 5 .:

head = first

tl = second

nil = f

isnil l = l (\h t d -> f) t

-- System F compatible encoding

nil' c n = n

-- T[\c -> \n -> n]
-- K T[\n -> n]      R3
-- K I               R4

nilSKI = k i

pair' h t c n = c h (t c n)

-- T[\h -> \t -> \c -> \n -> c h (t c n)]
-- T[\h -> T[\t -> T[\c -> T[\n -> c h (t c n)]]]]                        R555
-- T[\h -> T[\t -> T[\c -> S T[\n -> c h] T[\n -> t c n]]]]               R6
-- T[\h -> T[\t -> T[\c -> S (K (c h)) T[\n -> t c n]]]]                  R3211
-- T[\h -> T[\t -> T[\c -> S (K (c h)) (S T[\n -> t c] T[\n -> n])]]]     R6
-- T[\h -> T[\t -> T[\c -> S (K (c h)) (S T[\n -> t c] I)]]]              R4
-- T[\h -> T[\t -> T[\c -> S (K (c h)) (S (K (t c)) I)]]]                 R3211
-- pair2 = \h t c -> s (k (c h)) (s (k (t c)) i) works
-- T[\h -> T[\t -> S T[\c -> S (K (c h))] T[\c -> (S (K (t c)) I)]]]     R6
-- T[\h -> T[\t -> S (S T[\c -> S] T[\c -> (K (c h))]) T[\c -> (S (K (t c)) I)]]]     R6
-- T[\h -> T[\t -> S (S (K S) T[\c -> (K (c h))]) T[\c -> (S (K (t c)) I)]]]     R31
-- T[\h -> T[\t -> S (S (K S) (S T[\c -> K] T[\c -> (c h)])) T[\c -> (S (K (t c)) I)]]]     R6
-- T[\h -> T[\t -> S (S (K S) (S (K K) T[\c -> (c h)])) T[\c -> (S (K (t c)) I)]]]     R31
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S T[\c -> c] T[\c -> h]))) T[\c -> (S (K (t c)) I)]]]     R6
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) T[\c -> (S (K (t c)) I)]]]     R431
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) (S T[\c -> S (K (t c))] T[\c -> I])]]     R6
-- pair2 = \h t -> s (s (k s) (s (k k) (s i (k h)))) (s (\c -> s (k (t c))) (\c -> i)) works
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) (S T[\c -> S (K (t c))] (K I))]]     R31
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) (S (S T[\c -> S] T[\c -> (K (t c))]) (K I))]]   R6
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) (S (S (K S) T[\c -> (K (t c))]) (K I))]]   R31
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) (S (S (K S) (S T[\c -> K] T[\c -> (t c)])) (K I))]]   R6
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) (S (S (K S) (S (K K) T[\c -> (t c)])) (K I))]]   R31
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) (S (S (K S) (S (K K) (S T[\c -> t] T[\c -> c]))) (K I))]]   R6
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) (S (S (K S) (S (K K) (S (K t) T[\c -> c]))) (K I))]]   R31
-- T[\h -> T[\t -> S (S (K S) (S (K K) (S I (K h)))) (S (S (K S) (S (K K) (S (K t) I))) (K I))]]   R4
-- T[\h -> S T[\t -> S (S (K S) (S (K K) (S I (K h))))] T[\t -> (S (S (K S) (S (K K) (S (K t) I))) (K I))]]   R6
-- pair2 = \h -> s (\t -> s (s (k s) (s (k k) (s i (k h))))) (\t -> (s (s (k s) (s (k k) (s (k t) i))) (k i))) works
-- T[\h -> S T[\t -> S (S (K S) (S (K K) (S I (K h))))] T[\t -> (S (S (K S) (S (K K) (S (K t) I))) (K I))]]   R6
-- here can reduce first term either via application R6 or just constant R3, we do R3
-- T[\h -> S (K (S (S (K S) (S (K K) (S I (K h)))))) T[\t -> (S (S (K S) (S (K K) (S (K t) I))) (K I))]]   R3212212112212112211211

-- reduce T[\t -> (S (S (K S) (S (K K) (S (K t) I))) (K I))]
-- T[\t -> S (S (K S) (S (K K) (S (K t) I))) (K I)]
-- S T[\t -> S (S (K S) (S (K K) (S (K t) I)))] T[\t -> (K I)]  R6
-- S T[\t -> S (S (K S) (S (K K) (S (K t) I)))] (K (K I))      R3211
-- S (S T[\t -> S] T[\t -> (S (K S) (S (K K) (S (K t) I)))]) (K (K I))      R6
-- S (S (K S) T[\t -> (S (K S) (S (K K) (S (K t) I)))]) (K (K I))      R31
-- S (S (K S) (S T[\t -> S (K S)] T[\t -> (S (K K) (S (K t) I))])) (K (K I))      R6
-- S (S (K S) (S (K (S (K S))) T[\t -> (S (K K) (S (K t) I))])) (K (K I))      R321211
-- S (S (K S) (S (K (S (K S))) (S T[\t -> S (K K)] T[\t -> (S (K t) I)]))) (K (K I))      R6
-- S (S (K S) (S (K (S (K S))) (S (K (S (K K))) T[\t -> (S (K t) I)]))) (K (K I))      R321211
-- S (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S T[\t -> S (K t)] T[\t -> I])))) (K (K I))      R6
-- S (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S T[\t -> S (K t)] (K I))))) (K (K I))      R31
-- S (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (S T[\t -> S] T[\t -> (K t)]) (K I))))) (K (K I))      R6
-- S (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (S (K S) T[\t -> (K t)]) (K I))))) (K (K I))      R31
-- S (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (S (K S) (S T[\t -> K] T[\t -> t])) (K I))))) (K (K I))      R6
-- S (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (S (K S) (S (K K) I)) (K I))))) (K (K I))      R314
-- foo = S (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (S (K S) (S (K K) I)) (K I))))) (K (K I))

-- back to og term, reduce T[\h -> S (K (S (S (K S) (S (K K) (S I (K h)))))) foo

-- pair2 = \h -> s (k (s (s (k s) (s (k k) (s i (k h)))))) foo
--   where
--     foo = s (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (s (k s) (s (k k) i)) (k i))))) (k (k i))
-- works

-- T[\h -> S (K (S (S (K S) (S (K K) (S I (K h)))))) foo]
-- S T[\h -> S (K (S (S (K S) (S (K K) (S I (K h))))))] T[\h -> foo]  R6
-- S T[\h -> S (K (S (S (K S) (S (K K) (S I (K h))))))] (K foo)  R3 and a bunch of R12
-- S (S T[\h -> S] T[\h -> (K (S (S (K S) (S (K K) (S I (K h))))))]) (K foo)  R6
-- S (S (K S) T[\h -> (K (S (S (K S) (S (K K) (S I (K h))))))]) (K foo)  R31
-- S (S (K S) (S T[\h -> K] T[\h -> (S (S (K S) (S (K K) (S I (K h)))))])) (K foo)  R6
-- S (S (K S) (S (K K) T[\h -> (S (S (K S) (S (K K) (S I (K h)))))])) (K foo)  R31
-- S (S (K S) (S (K K) (S T[\h -> S] T[\h -> (S (K S) (S (K K) (S I (K h))))]))) (K foo)  R6
-- S (S (K S) (S (K K) (S (K S) T[\h -> (S (K S) (S (K K) (S I (K h))))]))) (K foo)  R31
-- S (S (K S) (S (K K) (S (K S) (S T[\h -> S (K S)] T[\h -> (S (K K) (S I (K h)))])))) (K foo)  R6
-- S (S (K S) (S (K K) (S (K S) (S (K (S (K S))) T[\h -> (S (K K) (S I (K h)))])))) (K foo)  R321211
-- S (S (K S) (S (K K) (S (K S) (S (K (S (K S))) (S T[\h -> S (K K)] T[\h -> (S I (K h))]))))) (K foo)  R6
-- S (S (K S) (S (K K) (S (K S) (S (K (S (K S))) (S (K (S (K K))) T[\h -> (S I (K h))]))))) (K foo)  R321211
-- S (S (K S) (S (K K) (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S T[\h -> S I] T[\h -> (K h)])))))) (K foo)  R6
-- S (S (K S) (S (K K) (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (K (S I)) T[\h -> (K h)])))))) (K foo)  R3211
-- S (S (K S) (S (K K) (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (K (S I)) (S T[\h -> K] T[\h -> h]))))))) (K foo)  R6
-- S (S (K S) (S (K K) (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (K (S I)) (S (K K) T[\h -> h]))))))) (K foo)  R31
-- S (S (K S) (S (K K) (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (K (S I)) (S (K K) I))))))) (K foo)  R4

-- pairSKI = s (s (k s) (s (k k) (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) (s (k k) i))))))) (k (s (s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (s (k s) (s (k k) i)) (k i))))) (k (k i))))

-- SHORTENED BY CORE.RUN
pairSKI :: a1 -> ((a1 -> b -> c) -> a2 -> b) -> (a1 -> b -> c) -> a2 -> c
pairSKI = s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k)))

(.:.) :: a1 -> ((a1 -> b -> c) -> a2 -> b) -> (a1 -> b -> c) -> a2 -> c
(.:.) = pairSKI

infixr 5 .:.

nrList :: [(a -> a) -> a -> a]
nrList = map encode [1 .. 5]

-- TODO reduce resulting SKI combinator

chToList :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
chToList fn = fn (:) []

listToCh :: [a] -> ((a -> b -> b) -> b -> b)
listToCh = foldr (.:.) nilSKI

pairSKI' ::
  SKI
    (a1 -> ((a1 -> b -> c) -> a2 -> b) -> (a1 -> b -> c) -> a2 -> c)
pairSKI' = S :- (K :- S) :- (S :- (K :- (S :- (K :- S))) :- (S :- (K :- (S :- (K :- K))) :- (S :- (K :- (S :- I)) :- K)))

nilSKI' :: SKI (b -> a -> a)
nilSKI' = K :- I

listToCh' :: [SKI a] -> SKI ((a -> b -> b) -> b -> b)
listToCh' = foldr (\cur acc -> pairSKI' :- cur :- acc) nilSKI'
