{-# LANGUAGE DataKinds, ViewPatterns #-}

module HeII where

import Data.Bifunctor
import GHC.TypeLits
import Data.Fixed
import Data.Proxy

data EN n

instance KnownNat n => HasResolution (EN n) where
    resolution proxy = 10^(natVal (Proxy :: Proxy n))

type FixedN n = Fixed (EN n)

type N = FixedN 42

c :: N
c = 299792458

h :: N
h = 0.000000000000000000000000000000000662607015

eV :: N
eV = 0.0000000000000000001602176634

λ :: N -> N
λ e = (c * h) / e

levels :: [N]
levels = (* eV) <$> [13.3, 13.2, 13.0, 12.7, 12.1, 10.2, 0]

main :: IO ()
main = mapM_ print . filter (\(snd -> x) -> x <= 750 && x >= 380) $ (second $ (\e -> (fromRational :: Rational -> FixedN 0) . toRational $ (10^9 * λ e))) <$> (filter ((> 0) . snd) $ (\x y -> ((x / eV, y / eV), x-y)) <$> levels <*> levels)