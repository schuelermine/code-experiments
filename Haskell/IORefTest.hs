module Main where

import Data.IORef

ds1 :: forall a b. (a, b) -> (b, b)
ds1 (x, y) = (y, y)

swp :: forall a b. (a, b) -> (b, a)
swp (x, y) = (y, x)

main :: IO ()
main = let
    main' pRef = do
        tRef <- newIORef ds1
        pVal <- readIORef pRef
        if snd (pVal (1, 2)) == 2
        then putStrLn ":)"
        else main' tRef
    in do
        pRef <- newIORef swp
        main' pRef