{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

import GHC.Generics (Generic)
import Test.QuickCheck
  ( Arbitrary
      ( arbitrary,
        shrink
      ),
    Gen,
    genericShrink,
    resize,
    sample,
    sized,
  )

data Tree f a
  = Leaf a
  | Node (f (Tree f a))
  deriving (Generic)

deriving instance
  ( forall b. Eq b => Eq (f b),
    Eq a
  ) =>
  Eq (Tree f a)

deriving instance
  ( forall b. Show b => Show (f b),
    Show a
  ) =>
  Show (Tree f a)

deriving instance
  ( forall b. Read b => Read (f b),
    Read a
  ) =>
  Read (Tree f a)

instance
  ( forall b. Arbitrary b => Arbitrary (f b),
    Arbitrary a
  ) =>
  Arbitrary (Tree f a)
  where
  arbitrary = sized \case
    0 -> Leaf <$> arbitrary
    size -> do
      l :: Bool <- arbitrary
      if l
        then Leaf <$> arbitrary
        else Node <$> resize (size - 1) arbitrary
  shrink = genericShrink

main :: IO ()
main = sample (arbitrary @(Tree [] ()))
