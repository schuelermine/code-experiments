{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Kind (Type)

data N where
    Z :: N
    S :: N -> N

data N_ n where
    Z_ :: N_ Z
    S_ :: N_ n -> N_ (S n)

data V t n where
    V0 :: V t Z
    V_ :: t -> V t n -> V t (S n)

data T t ds where
    T0 :: t -> T t '[]
    T_ :: V (T t ds) (S n) -> T t (s : ds)

type family Length ts where
    Length '[] = Z
    Length (_ : ts) = S (Length ts)

