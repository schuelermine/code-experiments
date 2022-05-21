import Data.List
import Data.Functor
import Data.Function
import System.Environment
i = concat . transpose
n = show <$> [0..] & (<&> \x -> even $ read [x]) . concat
t = True : False : (tail t >>= \x -> [x, not x])
main = getArgs >>= (\x -> putStrLn . genericTake x $ i [n, t] <&> \y -> if y then '?' else ' ') . read . head
