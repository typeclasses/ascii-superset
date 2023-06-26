module ASCII.SupersetConversion where

import ASCII.Superset (StringSuperset)
import Data.Kind (Type)

class (StringSuperset a, StringSuperset b) =>
    StringSupersetConversion (a :: Type) (b :: Type)
  where
    convertStringUnsafe :: a -> b
