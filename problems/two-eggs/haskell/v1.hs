{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

import Data.Bifunctor
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe

data N = Z | S N deriving (Show, Read, Eq, Ord)

instance Enum N where
  toEnum 0 = Z
  toEnum n
    | n < 0 = error "negative natural"
    | otherwise = S (toEnum (n - 1))
  fromEnum Z = 0
  fromEnum (S n) = 1 + fromEnum n

data F (n :: N) where
  FZ :: F n
  FS :: F n -> F ('S n)

deriving instance Show (F n)

deriving instance Eq (F n)

deriving instance Ord (F n)

class Fs (n :: N) where
  fs :: [F n]

instance Fs Z where
  fs = [FZ]

instance Fs n => Fs (S n) where
  fs = FZ : (FS <$> fs @n)

class FMax (n :: N) where
  fMax :: F n

instance FMax Z where
  fMax = FZ

instance FMax n => FMax (S n) where
  fMax = FS fMax

data family Strategy (eggs :: N) (floors :: N)

newtype instance Strategy Z f = FinallyDecide (F (S f))
  deriving (Show, Eq)

data instance Strategy (S e) f
  = DropEgg
      { floor :: F (S f),
        eggBreaks :: Strategy e f,
        eggSurvives :: Strategy (S e) f
      }
  | Decide (F (S f))

deriving instance Show (Strategy e f) => Show (Strategy (S e) f)

deriving instance Eq (Strategy e f) => Eq (Strategy (S e) f)

class Strategies e f where
  strategies :: N -> [Strategy e f]

instance Fs f => Strategies Z f where
  strategies Z = []
  strategies _ = FinallyDecide <$> fs

instance (Fs f, Strategies e f) => Strategies (S e) f where
  strategies Z = []
  strategies (S n) =
    (DropEgg <$> fs <*> strategies n <*> strategies n)
      ++ (Decide <$> fs)

data Result e f = Result
  { decidedFloor :: F (S f),
    brokenEggs :: F e,
    steps :: N
  }
  deriving (Show, Eq, Ord)

class PerformStrategy e f where
  performStrategy :: (?breakingPoint :: F (S f)) => Strategy e f -> Result e f

instance PerformStrategy Z f where
  performStrategy (FinallyDecide f) =
    Result
      { decidedFloor = f,
        brokenEggs = FZ,
        steps = Z
      }

instance PerformStrategy e f => PerformStrategy (S e) f where
  performStrategy (Decide f) =
    Result
      { decidedFloor = f,
        brokenEggs = FZ,
        steps = Z
      }
  performStrategy DropEgg {..} =
    if floor < ?breakingPoint
      then
        let next = performStrategy eggBreaks
         in next
              { brokenEggs = FS (next.brokenEggs),
                steps = S (next.steps)
              }
      else
        let next = performStrategy eggSurvives
         in next {steps = S (next.steps)}

checkStrategy :: (?breakingPoint :: F ('S f), PerformStrategy e f) => Strategy e f -> (Bool, Result e f)
checkStrategy strategy =
  let result = performStrategy strategy
   in (result.decidedFloor == ?breakingPoint, result)

checkStrategyAll :: (Fs f, PerformStrategy e f) => Strategy e f -> (Bool, [Result e f])
checkStrategyAll strategy =
  let results = fs <&> \n -> let ?breakingPoint = n in checkStrategy strategy
   in first and $ unzip results

fToNum :: Num a => F n -> a
fToNum FZ = 0
fToNum (FS n) = 1 + fToNum n

average :: forall a f. (Foldable f, Fractional a) => f a -> a
average =
  uncurry (/)
    . foldl' (\(!total, !count) x -> (total + x, count + 1)) (0, 0)

checkStrategyAvgSteps :: (Fractional c, Fs f, PerformStrategy e f) => Strategy e f -> (Bool, c)
checkStrategyAvgSteps strategy = second (average . fmap (fToNum . (.decidedFloor))) (checkStrategyAll strategy)

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f x y = case compare (f x) (f y) of
  LT -> x
  EQ -> x
  GT -> y

minX :: Ord p => p -> p -> p -> p
minX a b c = case compare a c of
  LT -> b
  EQ -> b
  GT -> c

minimumByFiltered :: Ord a => (c -> Maybe a) -> [c] -> c
minimumByFiltered f xs = let g x = (x, f x) in fst . minimumBy (compare `on` fromJust . snd) . filter (isJust . snd) $ g <$> xs

toMaybe :: Bool -> a -> Maybe a
toMaybe True a = Just a
toMaybe False _ = Nothing

fastestStrategy :: (Fs f, PerformStrategy e f, Strategies e f) => N -> Strategy e f
fastestStrategy n = minimumByFiltered (uncurry toMaybe . checkStrategyAvgSteps) $ strategies n
