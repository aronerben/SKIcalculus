module Nat where

import Core (SKI (..), i, k, reduce, s)
import Data.Function ((&))
import Data.List (deleteBy, find, foldl')

-- Naturals encoded as Church Numerals
-- https://en.wikipedia.org/wiki/Church_encoding#Church_numerals

type ChurchNumeral a = (a -> a) -> a -> a

-- Can't use this, as SKI (ChurchNumeral) would need impredicative polymorphism
-- type ChurchNumeral = forall a. (a -> a) -> a -> a

-- Zero
-- \f -> \x -> x

-- Use https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis to transform to SKI
-- T[\f -> \x -> x]
-- K T[\x -> x]      by R3
-- K I               by R4
czero :: ChurchNumeral a
czero = k i

-- Succ
-- Successor is built by composing functions
-- Zero := \f -> \x -> x
-- One  := \f -> \x -> f x
-- Two  := \f -> \x -> f (f x)
-- Succ := \(n : ChurchNumeral a) -> (\f -> \x -> f (n f x))
-- Use transformer
csucc :: ChurchNumeral a -> ChurchNumeral a
csucc = s (s (k s) k)

-- Readable to Church encoded
encode :: Integer -> ChurchNumeral a
encode 0 = czero
encode n = csucc $ encode $ pred n

decode :: ChurchNumeral Integer -> Integer
decode fn = fn succ 0

three :: Integer
three = decode $ csucc $ csucc $ csucc czero

fiftyfive :: Integer
fiftyfive = decode $ encode 55

-- To encode, "a" can be polymorphic since functions represent enumerables regardless of type
encodeEnum :: Enum a => a -> ChurchNumeral b
encodeEnum n | fromEnum n == 0 = czero
encodeEnum n = csucc $ encodeEnum $ pred n

decodeEnum :: Enum a => ChurchNumeral a -> a
decodeEnum fn = fn succ (toEnum 0)

-- transform :: ChurchNumeralhar -> ChurchNumeralnt
-- transform nr fn start = _

seven :: Integer
seven = decodeEnum $ encodeEnum (7 :: Integer)

newline :: Char
newline = decodeEnum $ encodeEnum '\n'

-- Add
(@+) :: ChurchNumeral a -> ChurchNumeral a -> ChurchNumeral a
(@+) = s (k s) (s (k (s (k s) k)))

infixl 6 @+

-- Mul
(@*) :: ChurchNumeral a -> ChurchNumeral a -> ChurchNumeral a
(@*) = s (k s) k

infixl 7 @*

-- Exp
(@^) :: ChurchNumeral a -> ChurchNumeral (a -> a) -> ChurchNumeral a
-- (@^) a b = instN $ (s (k (s i)) k) (N a) (N b)
-- TODO this just applies churchnumeral to itself...
(@^) = s (k (s i)) k

-- (@^) = undefined

infixr 8 @^

-- Halving
-- https://codegolf.stackexchange.com/questions/198840/ski-calculus-golf-half-of-a-church-numeral
halve ::
  ( ( ((((b1 -> c1) -> a1 -> b1) -> a2 -> c2) -> c3) ->
      (a2 -> ((b1 -> c1) -> a1 -> c1) -> c2) ->
      c3
    ) ->
    (((b2 -> a3 -> a3) -> (b3 -> a4 -> a4) -> c4) -> c4) ->
    (a5 -> b4 -> a5) ->
    c5
  ) ->
  c5
halve = s (s (s i (k (s (s (k s) k) (k (s (s (k s) (s (k k) s)) (k (s (k k) (s (s (k s) k))))))))) (k (s (s i (k (k i))) (k (k i))))) (k k)

example :: Integer
example = decode $ halve $ (e 2 @* e 2) @^ (e 3 @+ e 5)
  where
    e = encode

-- With show instance

type ChurchNumeral' a = (a -> a) -> a -> a

czero' :: SKI (ChurchNumeral' a)
czero' = K :- I

csucc' :: SKI (ChurchNumeral' a -> ChurchNumeral' a)
csucc' = S :- (S :- (K :- S) :- K)

encodeEnum' :: Enum a => a -> SKI (ChurchNumeral' b)
encodeEnum' n | fromEnum n == 0 = czero'
encodeEnum' n = csucc' :- encodeEnum' (pred n)

fiftyfive' :: SKI (ChurchNumeral' a)
fiftyfive' = encodeEnum' (55 :: Integer)

-- fiftyfive' in GHCi prints:
{-
((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) ((s ((s (k s)) k)) (k i))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-}
-- Using Show instance of SKI GADT

-- Can now use the printed result for pure SKI version
fiftyfive'' :: Integer
fiftyfive'' = decode $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))))))))))))))))))))))))))))))))))))))))))))))))))

seven' :: SKI (ChurchNumeral' a)
seven' = encodeEnum' (7 :: Int)

-- Following printed results from GHCi for pure SKI version
seven'' :: Int
seven'' = decodeEnum $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))

newline' :: SKI (ChurchNumeral' a)
newline' = encodeEnum' '\n'

newline'' :: Char
newline'' = decodeEnum $ s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))))))))

add' :: SKI (ChurchNumeral' a -> ChurchNumeral' a -> ChurchNumeral' a)
add' = S :- (K :- S) :- (S :- (K :- (S :- (K :- S) :- K)))

mul' :: SKI (ChurchNumeral' a -> ChurchNumeral' a -> ChurchNumeral' a)
mul' = S :- (K :- S) :- K

-- exp' :: SKI (ChurchNumeral' a -> ChurchNumeral' a -> ChurchNumeral' a)
exp' :: SKI (ChurchNumeral' a -> ChurchNumeral' (a -> a) -> ChurchNumeral' a)
-- TODO fix this
-- exp' :: SKI (b -> (b -> c) -> c)
exp' = S :- (K :- (S :- I)) :- K

-- exp' = undefined

halve' ::
  SKI
    ( ( ( ((((b1 -> c1) -> a1 -> b1) -> a2 -> c2) -> c3) ->
          (a2 -> ((b1 -> c1) -> a1 -> c1) -> c2) ->
          c3
        ) ->
        (((b2 -> a3 -> a3) -> (b3 -> a4 -> a4) -> c4) -> c4) ->
        (a5 -> b4 -> a5) ->
        c5
      ) ->
      c5
    )
halve' = S :- (S :- (S :- I :- (K :- (S :- (S :- (K :- S) :- K) :- (K :- (S :- (S :- (K :- S) :- (S :- (K :- K) :- S)) :- (K :- (S :- (K :- K) :- (S :- (S :- (K :- S) :- K))))))))) :- (K :- (S :- (S :- I :- (K :- (K :- I))) :- (K :- (K :- I))))) :- (K :- K)

example' :: SKI (ChurchNumeral' Int)
example' = halve' :- (exp' :- (mul' :- e (2 :: Int) :- e 2) :- (add' :- e 3 :- e 5))
  where
    e = encodeEnum'

-- From GHCi running example'
example'' :: Integer
example'' = decode $ s (s (s i (k (s (s (k s) k) (k (s (s (k s) (s (k k) s)) (k (s (k k) (s (s (k s) k))))))))) (k (s (s i (k (k i))) (k (k i))))) (k k) (s (s (k (s (k s) k)) s) (k k) i) (s (k s) k (s (s (k s) k) (s (s (k s) k) (k i))) (s (s (k s) k) (s (s (k s) k) (k i)))) (s (k s) k s (s (k s) k (s (k s) k)) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))))))

-- Represent 55 as 3^3 * 2 + 1 to shorten original
-- csucc' (mul' :- (exp' :- (e 3) :- (e 3)) :- (e 2))
fiftyfive''' :: Integer
fiftyfive''' = decode $ s (s (k s) k) (s (k s) k (s (s (k (s (k s) k)) s) (k k) i (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i)))) (s (s (k s) k) (s (s (k s) k) (s (s (k s) k) (k i))))) (s (s (k s) k) (s (s (k s) k) (k i))))

-- Find shortest formula
data Expr
  = (:+) Expr Expr
  | (:*) Expr Expr
  | (:^) Expr Expr
  | Succ Expr
  | Val Integer
  deriving (Show)

-- Need to use polymorphic because of exp'
exprToSKI :: Expr -> SKI (ChurchNumeral' a)
exprToSKI (a :+ b) = add' :- exprToSKI a :- exprToSKI b
exprToSKI (a :* b) = mul' :- exprToSKI a :- exprToSKI b
exprToSKI (a :^ b) = exp' :- exprToSKI a :- exprToSKI b
exprToSKI (Succ a) = csucc' :- exprToSKI a
exprToSKI (Val i) = encodeEnum' i

type Value = Integer

type Length = Integer

type Repr = String

type Entry = (Value, (Length, Expr))

-- type Dict = Map Value (Length, Expr)
type Dict = [Entry]

ops :: [(Value -> Value -> Value, Expr -> Expr -> Expr, Length -> Length -> Length)]
ops =
  -- Lengths calculated by hand for each SKI expression
  [ ((+), binExpr (:+), binLength 39)
  , ((*), binExpr (:*), binLength 19)
  , ((^), binExpr (:^), binLength 23)
  , (flip (^), flip $ binExpr (:^), binLength 23)
  , -- 20 (17 for succ, 2 for parens, 1 for space)
    (\cur _ -> succ cur, \cur _ -> Succ cur, \_ min' -> min' + 20)
  ]
  where
    binExpr op cur min' = cur `op` min'
    binLength len cur min' = len + cur + min'

-- Insert according to a comparator, assume the list is sorted, else unpredictable results
-- Good enough for this use case as we build up the list with this function
insertSortedBy :: (a -> a -> Bool) -> a -> [a] -> [a]
insertSortedBy cmp el (hd : tl)
  | cmp el hd = el : hd : tl
  | otherwise = hd : insertSortedBy cmp el tl
insertSortedBy _ el [] = [el]

-- TODO unfixed should be a Map that can return the minimal length
upsertUnfixed :: Dict -> Entry -> Dict
upsertUnfixed unfixed entry@(val, (len, _)) =
  updateUnfixed $ insertSortedBy (\(_, (len1, _)) (_, (len2, _)) -> len1 < len2) entry
  where
    updateUnfixed inserter = case find (\(curEntryVal, _) -> curEntryVal == val) unfixed of
      Just (_, (curEntryLength, _)) ->
        if curEntryLength > len
          then -- Element found in Dict that has worse length (longer)
          -- => Delete it and insert better entry
            deleteBy (\(v1, _) (v2, _) -> v1 == v2) entry unfixed & inserter
          else -- Element found in Dict that has better length
            unfixed
      -- Element not found => Insert it
      Nothing -> unfixed & inserter

-- One iteration of list growth/updating
step :: Value -> (Dict, Dict) -> (Dict, Dict)
step goal (fixed@((minEntryVal, (minEntryLength, minEntryRepr)) : _), unfixed) =
  let unfixedNew = Data.List.foldl' updatedUnfixed unfixed cartesian
   in (fixed, unfixedNew)
  where
    -- We work with every combo of operations and entries in fixed
    cartesian = [(op, entry) | op <- ops, entry <- fixed]
    -- Create new entry for current and minimal entry
    updatedUnfixed unfixedNew ((op, reprFn, weightFn), (curEntryVal, (curEntryLength, curEntryRepr))) = do
      let newVal = curEntryVal `op` minEntryVal
      let newLength = weightFn curEntryLength minEntryLength
      let newRepr = reprFn curEntryRepr minEntryRepr
      let newEntry = (newVal, (newLength, newRepr))
      -- Only add it to the list if below the goal
      -- This prevents sub and div from working, need other way to stop early
      if newVal > goal then unfixedNew else upsertUnfixed unfixedNew newEntry
step _ _ = error "Empty list"

-- Dijkstra's algorithm with growing graph
-- Technically can run just always with [0] as starting nrs (if we have succ in ops)
-- but slower
run' :: Value -> [Value] -> (Dict, Dict)
run' goal nrs = go ([], start)
  where
    --       Fix 0  Succs     Parens   Spaces
    --         V    V         V        V
    weight i = 5 + (i * 17) + (i * 2) + i
    -- Turn starting list into entries
    start = (\nr -> (nr, (weight nr, Val nr))) <$> nrs
    -- Take head of unfixed, turn into fixed and perform step with that
    go (fixed, hd@(v, _) : unfixed)
      -- Goal reached => Finish
      | v == goal = partitions
      | otherwise = go $ step goal partitions
      where
        partitions = (hd : fixed, unfixed)
    -- Unfixed is empty and we did not reach goal => Error
    go _ = error $ "Could not get to " <> show goal <> " with " <> show nrs

extract :: Value -> SKI (ChurchNumeral' Integer)
extract nr = reduce $ exprToSKI $ snd $ snd $ head $ fst $ run' nr [1 .. 20]