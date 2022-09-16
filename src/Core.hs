{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Core (s, k, i, SKI (S, K, I, (:-))) where

import Data.Text (Text)
import Data.Void
import Text.Parsec
import Text.Parsec.Text (Parser)

-- SKI combinators
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

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
reduce (K :- x :- y) = reduce x
reduce (S :- x :- y :- z) = reduce (x :- z :- (y :- z))
reduce (l :- r) = reduce l :- (reduce r)

-- Lambda calcus to SKI
data Expr
  = Var String
  | Abs String Expr
  | App Expr Expr
  deriving (Show, Eq)

-- Eta reduction
etas :: Expr -> Expr
etas = (until =<< ((==) =<<)) eta
  where
    eta :: Expr -> Expr
    eta (Abs s1 (App e (Var s2))) | s1 == s2 = e
    eta x@(Var s) = x
    eta (App e1 e2) = App (eta e1) (eta e2)
    eta (Abs s e) = Abs s (eta e)

-- See if a variable occurs free
free :: String -> Expr -> Bool
free var (Var s) = var == s
free var (Abs s e) = var /= s && free var e
free var (App e1 e2) = free var e1 || free var e2

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
  ident <- ident
  char '.'
  optional spaces
  Abs ident <$> expr

app :: Parser Expr
app = parens expr <|> abs' <|> var

expr :: Parser Expr
expr =
  chainl1 app $ do
    -- If has space, then it's application
    space
    pure App

parse' :: Text -> Either ParseError Expr
parse' = parse (expr <* eof) ""
