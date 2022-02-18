{-# LANGUAGE ImplicitParams #-}
import qualified Data.Map.Lazy as Map
import Data.Maybe
get :: (Ord k, ?m :: Map.Map k a) => k -> a -> a
get k a = fromMaybe a $ Map.lookup k ?m
kIs :: (Ord k, Eq a, ?m :: Map.Map k a) => k -> a -> Bool
kIs k a = case Map.lookup k ?m of
            Nothing -> False
            Just x  -> x == a
x :: Map.Map String String
x = let ?m = x
     in Map.fromList [
          (if kIs "a" "b" then "c" else "1", "c"),
          (if kIs "c" "c" then "b" else "2", "b"),
          ("a", get "b" "3")
        ]
main :: IO ()
main = print x
