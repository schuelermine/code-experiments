import qualified Data.Text as T
import qualified Data.Text.IO as T

fibo :: Integral a => a -> a -> a
fibo = fiboh 1 0 0 

fiboh :: Integral a => a -> a -> a -> a -> a -> a
fiboh c p i n m
  | i == n      = m `seq` p `seq` c
  | otherwise   = fiboh ((c + p) `mod` m) c (i + 1) n m

main =
  do
    n <- map read <$> (words . T.unpack <$> T.getLine) :: IO [Int]
    T.putStrLn $ T.pack . show $ fibo (head n) $ head . tail $ n


f :: Integral a => a -> a -> a
f n m = g 1 0 0 where
    g c p i
        | i == n = c
        | otherwise = g (mod (c + p) m) c (i + 1)

main = do
    n <- (read <$>) . words . T.unpack  
