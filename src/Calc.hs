{-# LANGUAGE GADTs #-}

module Calc where

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
-- ((compose f) (fstack f)) nr =
-- (f . (fstack f)) nr =
-- fstack is the function composition stack which collapses after s combinator is applied
-- (f . (f . f ... f)) nr =
csucc ::  ((b -> c) -> a -> b) -> (b -> c) -> a -> c
csucc = s (s (k s) k)

encode :: Int -> (a -> a) -> a -> a
encode 0 = czero
encode n = csucc $ encode (n-1)

decode :: ((Int -> Int) -> Int -> a) -> a
decode fn = fn succ 0

-- Encoded as value constructors
data SKI e where
  S :: SKI ((a -> b -> c) -> (a -> b) -> a -> c)
  K :: SKI (a -> b -> a)
  I :: SKI (a -> a)
  (:-) :: SKI (a -> b) -> SKI a -> SKI b
  (:+) :: SKI a -> SKI b -> SKI (a -> b)

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




-- Scratchpad
-- data SKI = S (SKI, SKI, SKI) | K (SKI, SKI) | I SKI | V
--   deriving (Show)


-- -- eval (I x) = eval x
-- -- eval (K x y) = eval x
-- -- -- eval (S x y z)

-- data SKI2 = K2 (SKI2 -> SKI2) | I2 SKI2

-- data SKI3 ph where
--   K3 :: SKI3 (a -> b -> a)
--   I3 :: SKI3 (a -> a)
--   (:$) :: SKI3 (a -> b) -> SKI3 a -> SKI3 b


-- data SKI4 = S4 | K4 | I4
--   deriving (Show)

-- data SKI5 = S5 SKI5 SKI5 SKI5 | K5 SKI5 SKI5 | I5 SKI5 | V5
--   deriving (Show)

-- newtype Mu a = Mu (Mu a -> a)
-- y f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)

-- data SKI6 = S6 | K6 | I6 | T6 (SKI6, SKI6)
--   deriving (Show)
