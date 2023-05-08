module List where

import Core (SKI (..), i, k, s)
import Nat (ChurchNumeral, encodeEnum)

nilSKI :: a -> b -> b
nilSKI = k i

pairSKI :: a1 -> ((a1 -> b -> c) -> a2 -> b) -> (a1 -> b -> c) -> a2 -> c
pairSKI = s (k s) (s (k (s (k s))) (s (k (s (k k))) (s (k (s i)) k)))

(.:.) :: a1 -> ((a1 -> b -> c) -> a2 -> b) -> (a1 -> b -> c) -> a2 -> c
(.:.) = pairSKI

infixr 5 .:.

nrList :: [ChurchNumeral Int]
nrList = map encodeEnum [(1 :: Integer) .. 5]

chToList :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
chToList fn = fn (:) []

listToCh :: [a] -> ((a -> b -> b) -> b -> b)
listToCh = foldr (.:.) nilSKI

nilSKI' :: SKI (b -> a -> a)
nilSKI' = K :- I

pairSKI' ::
  SKI
    (a1 -> ((a1 -> b -> c) -> a2 -> b) -> (a1 -> b -> c) -> a2 -> c)
pairSKI' = S :- (K :- S) :- (S :- (K :- (S :- (K :- S))) :- (S :- (K :- (S :- (K :- K))) :- (S :- (K :- (S :- I)) :- K)))

listToCh' :: [SKI a] -> SKI ((a -> b -> b) -> b -> b)
listToCh' = foldr (\cur acc -> pairSKI' :- cur :- acc) nilSKI'
