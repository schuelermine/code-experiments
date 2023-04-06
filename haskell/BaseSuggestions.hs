module Suggestions where

import Control.Applicative

clamp :: Ord c => c -> c -> c -> c
clamp n x = min n . max x

nTimes :: Integral i => i -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

combinations :: Integral i => [a] -> i -> [[a]]
combinations x i = nTimes i ((:) <$> x <*>) [[]]
