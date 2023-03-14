module Interview where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (tails)

type WindowSize = Int

data Error = SizeNonPos
  deriving (Eq, Show)

-- Don't average until WindowSize is reached
smooth :: WindowSize -> [Double] -> Either Error [Double]
smooth size numbers
  | size <= 0 = Left SizeNonPos
  | otherwise =
    Right $
      subLists
        <&> ((/ fromIntegral size) . sum)
  where
    subLists =
      [take size ls | ls <- tails numbers, length ls >= size]

testList :: [Double]
testList = [1.0 .. 10.0]

testLength :: Bool
testLength = (length <$> smooth 3 testList) == Right 8

testZeroNeg :: Bool
testZeroNeg = smooth 0 testList == Left SizeNonPos && smooth (-2) testList == Left SizeNonPos

testSmoothing :: Bool
testSmoothing = smooth 3 testList == Right [2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
