module Core2 where

import Data.List (sortBy)
import Text.Printf (PrintfArg, PrintfType, printf)

-- data Expr
--   = Var String
--   | Abs String Expr
--   | App Expr Expr
--   deriving (Show, Eq)

-- makeBaseFunctor ''Expr

-- freeVars :: Expr -> Set String
-- freeVars = cata go
--   where
--     go :: ExprF (Set String) -> Set String
--     go (VarF v) = Set.singleton v
--     go (AbsF v e) = Set.difference e (Set.singleton v)
--     go (AppF e1 e2) = Set.union e1 e2

-- free :: String -> Expr -> Bool
-- free var expr = Set.member var $ freeVars expr

-- data SKI
--   = S
--   | K
--   | I
--   | Appy SKI SKI
--   | Arg
--   deriving (Show)

-- -- -- TODO doesnt work
-- -- transform :: Expr -> SKI
-- -- transform = para go
-- --   where
-- --     go :: ExprF (Expr, SKI) -> SKI
-- --     go (VarF _) = Arg
-- --     go (AppF (_, n1) (_, n2)) = Appy n1 n2
-- --     go (AbsF v (eo, e)) | not (free v eo) = Appy K e
-- --     go (AbsF v1 (Var v2, _)) | v1 == v2 = I
-- --     go (AbsF v (Abs _ eo, e)) | free v eo = e
-- --     go (AbsF v (App e1 e2, e))
-- --       | free v e1 || free v e2 =
-- --         Appy (Appy S (go $ AbsF v (e1, _))) (go $ AbsF v (e2, _))
-- --     go (AbsF _ (n, _)) = error $ "abs" <> show n

-- makeBaseFunctor ''SKI

-- nested :: Expr -> SKI
-- nested = apo go
--   where
--     go (Var s) = ArgF
--     go (App e1 e2) = AppyF (Right e1) (Right e2)
--     go (Abs v e) | not (free v e) = AppyF (Left K) (Right e)
--     go (Abs v1 (Var v2)) | v1 == v2 = IF

-- -- go x@(Abs v1 (Abs v2 e)) | free v1 e = Right e

-- --     go (Abs v (App e1 e2)) | free v e1 || free v e2 = let fs1 = AppyF (Left S) (Right $ Abs v e1) in AppyF _ (Right $ Abs v e1)

-- nested' :: Expr -> SKI
-- nested' = futu go
--   where
--     go :: Expr -> SKIF (Free SKIF Expr)
--     go (Var _) = ArgF
--     go (App e1 e2) = AppyF (Pure e1) (Pure e2)
--     go (Abs v e) | not $ free v e = AppyF (Free KF) (Pure e)
--     go (Abs v1 (Var v2)) | v1 == v2 = IF
--     -- go (Abs v1 e) = go e
--     -- go (Abs v1 (Abs v2 e))
--     --   | free v1 e =
--     --     let (Free g) = foo in g
--     --   where
--     --     foo = Free (AbsF v1 (Free (AbsF v2 $ Pure e)))
--     go (Abs v (App e1 e2))
--       | free v e1 || free v e2 =
--         let fs1 = Free $ AppyF (Free SF) (Pure $ Abs v e1)
--          in AppyF fs1 (Pure $ Abs v e1)

-- -- go foo = foo >>= go

-- rld0 :: [(Int, a)] -> [a]
-- rld0 = futu dec

-- dec [] = Nil
-- dec ((n, c) : xs) = let (Free g) = rep n in g
--   where
--     rep 0 = Pure xs
--     rep m = Free (Cons c (rep (m - 1)))

-- -- go (Abs v e) | not (free v e) = let (Free g) = rep e in g
-- --   where
-- --     rep m = _

-- -- go (Abs v1 (Var v2)) | v1 == v2 = Left <$> IF

-- -- go (Abs v (App e1 e2)) | free v e1 || free v e2 = let fs1 = AppyF (Left S) (Right $ Abs v e1) in AppyF _ (Right $ Abs v e1)

-- -- go x@(Abs v1 (Abs v2 e)) | free v1 e = Right x

-- -- general :: Expr -> SKI
-- -- general (Var _) = Arg
-- -- general (App e1 e2) = Appy (general e1) (general e2)
-- -- general (Abs v e) | not (free v e) = Appy K $ general e
-- -- general (Abs v1 (Var v2)) | v1 == v2 = I
-- -- general (Abs v1 (Abs v2 e)) | free v1 e = general (Abs v1 $ general (Abs v2 e))

-- -- general (Abs v (App e1 e2, e)) | free v e1 || free v e2 = Appy (Appy S (go $ AbsF v (e1, _))) (go $ AbsF v (e2, _))
-- -- general (AbsF _ (n, _)) = error $ "abs" <> show n

-- TODO add Succ
data Expr
  = (:+) Expr Expr
  | (:*) Expr Expr
  | (:^) Expr Expr
  | Val Integer

print' ::
  ( Text.Printf.PrintfArg t1
  , Text.Printf.PrintfType t2
  , Show a1
  , Show a2
  ) =>
  a1 ->
  t1 ->
  a2 ->
  t2
print' l op r = printf "(%s %s %s)" (show l) op (show r)

instance Show Expr where
  show (a :+ b) = print' a "+" b
  show (a :* b) = print' a "*" b
  show (a :^ b) = print' a "^" b
  show (Val i) = show i

-- Do not give precedence on purpose, want left to right evaluation

extract :: Expr -> Integer
extract (a :+ b) = extract a + extract b
extract (a :* b) = extract a * extract b
extract (a :^ b) = extract a ^ extract b
extract (Val i) = i

rules :: Expr -> Bool
rules (Val 0 :* _) = False
rules (Val 1 :* _) = False
rules (_ :* Val 1) = False
rules (Val 0 :^ _) = False
rules (Val 1 :^ _) = False
rules (_ :^ Val 1) = False
rules _ = True

vals :: [Integer]
vals = reverse [1 .. 9]

type Op = Expr -> Expr -> Expr

ops :: [Op]
ops = [(:+), (:*), (:^)]

raw ::
  (Integer, Expr) ->
  Integer ->
  Op ->
  Integer ->
  Integer ->
  [(Integer, Expr)]
raw acc val op n len
  | res >= n || len == 0 = [acc']
  | otherwise =
    concat
      [ raw acc' val' op' n (len - 1)
      | op' <- ops
      , -- TODO WRONG
      -- , val' <- if op' == Exp then vals else dropWhile (> val) vals
      -- TODO maybe check for current sum what the min weight is, if curr is higher, abort
      val' <- vals
      , rules $ op' (Val $ fst acc) (Val val')
      ]
  where
    entry = op (Val $ fst acc) (Val val)
    res = extract entry
    acc' = (res, op (snd acc) (Val val))

run :: Integer -> Integer -> [(Integer, Expr)]
run n maxOps =
  Prelude.filter ((== n) . fst) $
    concat
      [ raw (start, Val start) val' op' n (maxOps - 1)
      | val' <- vals
      , op' <- ops
      , start <- vals
      , rules $ op' (Val start) (Val val')
      ]

weight :: Expr -> Integer
weight (a :+ b) = weight a + weight b + 55
weight (a :* b) = weight a + weight b + 19
weight (a :^ b) = weight a + weight b + 47
-- --          Fix Succs      Parens    Spaces
-- --          V   V          V         V
weight (Val i) = 5 + i * 17 + i * 2 + i

foo = Prelude.take 10 $ sortBy (\(_, e1) (_, e2) -> compare (weight e1) (weight e2)) $ run 55 5
