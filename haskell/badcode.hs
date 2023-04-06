module Data.Priority (Priority(..), compareBase) where

data Priority t = Base t | Higher (Priority t) | Lower (Priority t)

instance Ord b => Ord (Priority b) where
    compare (Lower p) (Lower q) = compare p q
    compare (Lower p) (Base b) = ifEQ LT (compareBase p b)
    compare (Lower p) (Higher q) = ifEQ LT (compare p q)
    compare (Base a) (Lower q) = ifEQ GT (flipOrder $ compareBase q a)
    compare (Base a) (Base b) = compare a b
    compare (Base a) (Higher q) = ifEQ LT (flipOrder $ compareBase q a) 
    compare (Higher p) (Lower q) = ifEQ GT (compare p q)
    compare (Higher p) (Base b) = ifEQ GT (compareBase p a)
    compare (Higher p) (Higher q) = compare p q
   
compareBase :: Ord t => Priority t -> t -> Ordering
compareBase a (Lower b) = ifEQ GT (compareBase a b)
compareBase a (Base b) = compare a b
compareBase a (Higher b) = ifEQ LT (compareBase a b)

ifEQ :: Ordering -> Ordering -> Ordering
ifEQ base ordering = case ordering of
    EQ -> base
    x -> x

flipOrder :: Ordering -> Ordering
flipOrder ordering = case ordering of
    LT -> GT
    EQ -> EQ
    GT -> LT
