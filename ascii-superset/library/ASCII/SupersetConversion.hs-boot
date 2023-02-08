module ASCII.SupersetConversion where

import ASCII.Superset (StringSuperset)

class (StringSuperset a, StringSuperset b) => StringSupersetConversion a b where
    convertStringUnsafe :: a -> b
