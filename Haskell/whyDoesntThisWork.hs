{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

data family Foo (b :: Bool)
data instance Foo True = FooBar
data instance Foo False = FooBaz

foo :: Foo b -> Bool
foo FooBar = True
foo FooBaz = False
