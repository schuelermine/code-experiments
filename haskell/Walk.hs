import Control.Applicative

(!!*) :: (Foldable f, Integral i) => f a -> i -> Maybe a
structure !!* targetIndex = g $ foldl f (Left 0) structure
  where
    f it@(Right value) _ = it
    f (Left lastIndex) nextValue
      | lastIndex == targetIndex = Right nextValue
      | otherwise = Left $ succ lastIndex
    g (Left _) = Nothing
    g (Right value) = Just value

tacnocA :: (Traversable f, Applicative g, Foldable g, Alternative f) => f (g a) -> f a
tacnocA structure = foldl (<|>) empty $ sequenceA structure
