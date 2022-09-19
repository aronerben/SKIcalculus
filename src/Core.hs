{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Core (s, k, i, SKI (S, K, I, (:-))) where

import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Printf (printf)

-- SKI combinators
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x _ = x

-- Can be constructed from s and k, not a primitive
i :: a -> a
i = s k k

-- SKI types encoded as GADT
data SKI e where
  S :: SKI ((a -> b -> c) -> (a -> b) -> a -> c)
  K :: SKI (a -> b -> a)
  I :: SKI (a -> a)
  -- Corresponds to function application
  (:-) :: SKI (a -> b) -> SKI a -> SKI b

-- Show instance to transform to function encoding
-- Use this to generate long SKI sequences
instance Show (SKI e) where
  show S = "s"
  show K = "k"
  show I = "i"
  show (a :- b) = "(" ++ show a ++ " " ++ show b ++ ")"

reduce :: SKI a -> SKI a
reduce S = S
reduce K = K
reduce I = I
reduce (I :- x) = reduce x
reduce (K :- x :- _) = reduce x
reduce (S :- x :- y :- z) = reduce (x :- z :- (y :- z))
reduce (l :- r) = reduce l :- reduce r

-- Lambda calculus to SKI
data Expr
  = Var String
  | Abs String Expr
  | App Expr Expr
  | S'
  | K'
  | I'
  deriving (Show, Eq)

-- Check if variable occurs free
free :: String -> Expr -> Bool
free var' (Var v) = var' == v
free var' (Abs v e) = var' /= v && free var' e
free var' (App e1 e2) = free var' e1 || free var' e2
free _ S' = False
free _ K' = False
free _ I' = False

-- Eta reduction
etas :: Expr -> Expr
etas = (until =<< ((==) =<<)) eta
  where
    eta :: Expr -> Expr
    -- Reduce if form is \x. F x and x does not occur free in F
    eta (Abs v1 (App e (Var v2))) | v1 == v2 && not (free v1 e) = e
    eta x@(Var _) = x
    eta (App e1 e2) = App (eta e1) (eta e2)
    eta (Abs v e) = Abs v (eta e)
    eta S' = S'
    eta K' = K'
    eta I' = I'

-- TODO maybe use GADT to encode SKI into Expr?
-- TODO run etas
-- TODO write tests
-- TODO mention in blogpost that there are many more bracket abstractions
-- TODO use Writer to see which rule applied
transform :: Expr -> Expr
-- Case 1
transform x@(Var _) = x
transform S' = S'
transform K' = K'
transform I' = I'
-- Case 2
transform (App e1 e2) = App (transform e1) (transform e2)
-- Case 3
transform (Abs v e) | not (free v e) = App K' (transform e)
-- Case 4
transform (Abs v1 (Var v2)) | v1 == v2 = I'
-- Case 5
transform (Abs v1 (Abs v2 e))
  | free v1 e = transform $ Abs v1 $ transform $ Abs v2 e
-- Case 6
transform (Abs v (App e1 e2)) =
  App (App S' $ transform $ Abs v e1) $ transform $ Abs v e2
transform _ = error "Should not happen"
  where
    transform' = etas . transform

-- Parser
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

ident :: Parser String
ident = do
  initialChar <- letter
  rest <- many (alphaNum <|> char '_')
  pure $ initialChar : rest

var :: Parser Expr
var = Var <$> ident

abs' :: Parser Expr
abs' = do
  char '/'
  ident' <- ident
  char '.'
  optional spaces
  Abs ident' <$> expr

app :: Parser Expr
app = parens expr <|> abs' <|> var

expr :: Parser Expr
expr =
  -- If has space, then it's application
  chainl1 app $ space >> pure App

parse' :: Text -> Either ParseError Expr
parse' = parse (expr <* eof) ""

-- Pretty print
pp :: Expr -> String
pp (Var v) = v
pp (Abs v e) = printf "/%s.%s" v $ pp e
pp (App e1 e2) = printf "(%s %s)" (pp e1) (pp e2)
pp S' = "s"
pp K' = "k"
pp I' = "i"
