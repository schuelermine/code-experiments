hyper 0 _ b = b + 1
hyper 1 a 0 = a
hyper 2 a 0 = 0
hyper _ a 0 = 1
hyper n a b = hyper (n - 1) a (hyper n a (b - 1))
