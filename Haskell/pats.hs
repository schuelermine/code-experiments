{-# LANGUAGE ImplicitParams, BlockArguments #-}

import Data.List
import Control.Monad.State

grid :: (?dimensions :: (Integer, Integer)) => [(Integer, Integer)]
grid = (,) <$> [1..(fst ?dimensions)] <*> [1..(snd ?dimensions)]

advpc :: (?dimensions :: (Integer, Integer)) => [[(Integer, Integer)]] -> State [[(Integer, Integer)]] [[(Integer, Integer)]]
advpc curpatcs =
    let nextpatcs = curpatcs >>= \patc -> (: patc) <$> grid \\ patc
    in state \oldpatcs ->
        (nextpatcs, oldpatcs ++ curpatcs)

getpcs :: (?dimensions :: (Integer, Integer)) => Integer -> [[(Integer, Integer)]]
getpcs n =
    let f = foldr (>=>) return (genericReplicate n advpc)
    in uncurry (++) $ runState (return [[]] >>= f) []

ispat :: [(Integer, Integer)] -> Bool
--ispat 