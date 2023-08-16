{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoStarIsType #-}

module T where

import Data.Kind

data N :: Type where
    Z :: N
    S :: N -> N

data T r t where
    T0 :: t -> T 'Z t
    TD :: T r t -> T ('S r) t -> T ('S r) t
    TR :: T r t -> T r t -> T ('S r) t

