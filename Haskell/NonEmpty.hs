module Data.NonEmpty (
  NonEmpty(),
  getNonEmpty
) where

data NonEmpty f a = NonEmpty { getNonEmpty :: (f a) }

nonEmpty :: Foldable f => f a -> Maybe (NonEmpty f a)
nonEmpty xs
  | null xs = Nothing
  | otherwise = Just $ NonEmpty xs

