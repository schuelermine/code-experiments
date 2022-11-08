{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

data family Foo (b :: Bool)
data instance Foo True = FooBar
data instance Foo False = FooBaz

class FooFoo (b :: Bool) where
    foo :: Foo b -> Bool

instance FooFoo True where
    foo FooBar = True

instance FooFoo False where
    foo FooBaz = False

-- foo2 :: Foo b -> Bool
-- foo2 FooBar = True
-- foo2 FooBaz = False
