import Data.Kind
f :: forall (q :: Type -> Constraint) t. (forall a. q a => a -> t) -> forall b. q b => t
f g = let j :: forall b. q b => t
          j = (g :: q b => b -> t) undefined
      in j