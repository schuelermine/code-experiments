select :: (Int -> Bool) -> [a] -> [a]
select f = fmap snd . filter (f . fst) . zip [0 ..]

evalLagrangePoly :: Fractional a => [(a, a)] -> a -> a
evalLagrangePoly ps x =
  sum
    [ py
        * product
          [ (x - px')
              / (px - px')
            | (_, (px', _)) <- filter ((/= i) . fst) ips
          ]
      | (i, (px, py)) <- ips
    ]
  where
    ips = zip [0 ..] ps
