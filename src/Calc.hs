{-# LANGUAGE GADTs #-}

module Calc where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i = s k k

t :: a -> b -> a
t = k

f :: a -> b -> b
f x = s k (const x)

n :: ((a -> b -> b) -> (c -> d -> c) -> e) -> e
n = s (s i (k f)) (k t)

a :: (a -> (b -> c -> c) -> d) -> a -> d
a = s s (k (k f))

o :: ((a -> b -> a) -> c) -> c
o = s i (k t)

-- Failed type level tries
data SKI = S (SKI, SKI, SKI) | K (SKI, SKI) | I SKI | V
  deriving (Show)


-- eval (I x) = eval x
-- eval (K x y) = eval x
-- -- eval (S x y z)

data SKI2 = K2 (SKI2 -> SKI2) | I2 SKI2

data SKI3 ph where
  K3 :: SKI3 (a -> b -> a)
  I3 :: SKI3 (a -> a)
  (:$) :: SKI3 (a -> b) -> SKI3 a -> SKI3 b


data SKI4 = S4 | K4 | I4
  deriving (Show)

data SKI5 = S5 SKI5 SKI5 SKI5 | K5 SKI5 SKI5 | I5 SKI5 | V5
  deriving (Show)
