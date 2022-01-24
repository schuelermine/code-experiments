Here’s how I want this to work. Let’s call our Monad `MTX` (for no reason)

> unMTX :: MTX s m x -> s -> (m a, s)
> mtx :: (s -> (m a, s)) -> MTX s m x

Given:

> {-# LANGUAGE BlockArguments #-}
> foo :: MTX s m x
> bar :: x -> MTX s m y

Then, the sought-after monad would transform:

> unMTX do
>   x <- foo
>   bar x

into

> \state1 ->
>   let (results1, state2) = foo state1
>   in 