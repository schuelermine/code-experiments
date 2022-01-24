import Data.List
import Data.Functor
import Data.Function
import System.Environment
i = concat . transpose
n = show <$> [0..] & (<&> \x -> even $ read [x]) . concat
t = True : False : (tail t >>= \x -> [x, not x])
j = i [n, t]
m l = s 1 l where
    s n h = (length . filter id $ take n h) : s (n + 1) (drop n h)
k = m j
main = read . head <$> getArgs >>= \x -> mapM_ (putStrLn . show) $ genericTake x k
