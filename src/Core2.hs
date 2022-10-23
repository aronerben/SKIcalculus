{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Core2 where

import Control.Monad.Free
import Data.Functor.Base (ListF)
import Data.Functor.Foldable (ListF (..), ana, apo, cata, futu, para)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Set as Set

data Expr
  = Var String
  | Abs String Expr
  | App Expr Expr
  deriving (Show, Eq)

makeBaseFunctor ''Expr

freeVars :: Expr -> Set String
freeVars = cata go
  where
    go :: ExprF (Set String) -> Set String
    go (VarF v) = Set.singleton v
    go (AbsF v e) = Set.difference e (Set.singleton v)
    go (AppF e1 e2) = Set.union e1 e2

free :: String -> Expr -> Bool
free var expr = Set.member var $ freeVars expr

data SKI
  = S
  | K
  | I
  | Appy SKI SKI
  | Arg
  deriving (Show)

-- -- TODO doesnt work
-- transform :: Expr -> SKI
-- transform = para go
--   where
--     go :: ExprF (Expr, SKI) -> SKI
--     go (VarF _) = Arg
--     go (AppF (_, n1) (_, n2)) = Appy n1 n2
--     go (AbsF v (eo, e)) | not (free v eo) = Appy K e
--     go (AbsF v1 (Var v2, _)) | v1 == v2 = I
--     go (AbsF v (Abs _ eo, e)) | free v eo = e
--     go (AbsF v (App e1 e2, e))
--       | free v e1 || free v e2 =
--         Appy (Appy S (go $ AbsF v (e1, _))) (go $ AbsF v (e2, _))
--     go (AbsF _ (n, _)) = error $ "abs" <> show n

makeBaseFunctor ''SKI

nested :: Expr -> SKI
nested = apo go
  where
    go (Var s) = ArgF
    go (App e1 e2) = AppyF (Right e1) (Right e2)
    go (Abs v e) | not (free v e) = AppyF (Left K) (Right e)
    go (Abs v1 (Var v2)) | v1 == v2 = IF

-- go x@(Abs v1 (Abs v2 e)) | free v1 e = Right e

--     go (Abs v (App e1 e2)) | free v e1 || free v e2 = let fs1 = AppyF (Left S) (Right $ Abs v e1) in AppyF _ (Right $ Abs v e1)

nested' :: Expr -> SKI
nested' = futu go
  where
    go :: Expr -> SKIF (Free SKIF Expr)
    go (Var _) = ArgF
    go (App e1 e2) = AppyF (Pure e1) (Pure e2)
    go (Abs v e) | not $ free v e = AppyF (Free KF) (Pure e)
    go (Abs v1 (Var v2)) | v1 == v2 = IF
    -- go (Abs v1 e) = go e
    -- go (Abs v1 (Abs v2 e))
    --   | free v1 e =
    --     let (Free g) = foo in g
    --   where
    --     foo = Free (AbsF v1 (Free (AbsF v2 $ Pure e)))
    go (Abs v (App e1 e2))
      | free v e1 || free v e2 =
        let fs1 = Free $ AppyF (Free SF) (Pure $ Abs v e1)
         in AppyF fs1 (Pure $ Abs v e1)

-- go foo = foo >>= go

rld0 :: [(Int, a)] -> [a]
rld0 = futu dec

dec [] = Nil
dec ((n, c) : xs) = let (Free g) = rep n in g
  where
    rep 0 = Pure xs
    rep m = Free (Cons c (rep (m - 1)))

-- go (Abs v e) | not (free v e) = let (Free g) = rep e in g
--   where
--     rep m = _

-- go (Abs v1 (Var v2)) | v1 == v2 = Left <$> IF

-- go (Abs v (App e1 e2)) | free v e1 || free v e2 = let fs1 = AppyF (Left S) (Right $ Abs v e1) in AppyF _ (Right $ Abs v e1)

-- go x@(Abs v1 (Abs v2 e)) | free v1 e = Right x

-- general :: Expr -> SKI
-- general (Var _) = Arg
-- general (App e1 e2) = Appy (general e1) (general e2)
-- general (Abs v e) | not (free v e) = Appy K $ general e
-- general (Abs v1 (Var v2)) | v1 == v2 = I
-- general (Abs v1 (Abs v2 e)) | free v1 e = general (Abs v1 $ general (Abs v2 e))

-- general (Abs v (App e1 e2, e)) | free v e1 || free v e2 = Appy (Appy S (go $ AbsF v (e1, _))) (go $ AbsF v (e2, _))
-- general (AbsF _ (n, _)) = error $ "abs" <> show n
