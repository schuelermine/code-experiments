{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}

import Data.Function
import Data.List

data N = Z | S N deriving (Show, Read, Eq, Ord)
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
  fs = [FZ] ++ (FS <$> fs @n)

class FMax (n :: N) where
  fMax :: F n

instance FMax Z where
  fMax = FZ

instance FMax n => FMax (S n) where
  fMax = FS fMax

data family Strategy (eggs :: N) (floors :: N)
data instance Strategy Z f = FinallyDecide (F (S f))
  deriving (Show, Eq)
data instance Strategy (S e) f
  = DropEgg {
      floor :: F (S f),
      eggBreaks :: Strategy e f,
      eggSurvives :: Strategy (S e) f }
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

data Result e f = Result {
  decidedFloor :: F (S f),
  brokenEggs :: F e,
  steps :: N }
      
class PerformStrategy e f where
  performStrategy :: (?breakingPoint :: F (S f)) => Strategy e f -> Result e f

instance PerformStrategy Z f where
  performStrategy (FinallyDecide f) = Result {
    decidedFloor = f,
    brokenEggs = FZ,
    steps = Z }

instance PerformStrategy e f => PerformStrategy (S e) f where
  performStrategy (Decide f) = Result {
    decidedFloor = f,
    brokenEggs = FZ,
    steps = Z }
  performStrategy DropEgg {..}
    = if floor < ?breakingPoint
      then let next = performStrategy eggBreaks
        in next {
          brokenEggs = FS (next.brokenEggs),
          steps = S (next.steps) }
      else let next = performStrategy eggSurvives
        in next {steps = S (next.steps)}