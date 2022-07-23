{-# LANGUAGE PatternSynonyms #-}

module Data.NonEmpty (
  NonEmpty()
) where

data NonEmpty f a = NonEmpty (f a)

nonEmpty :: Foldable f => f a -> Maybe (NonEmpty f a)
nonEmpty xs
  | null xs = Nothing
  | otherwise = Just $ NonEmpty xs

