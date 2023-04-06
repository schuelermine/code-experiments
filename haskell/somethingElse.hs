import System.Environment
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "no arguments"
    x:_ -> error "too many arguments"
