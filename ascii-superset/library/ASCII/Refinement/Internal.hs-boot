module ASCII.Refinement.Internal where

import {-# source #-} ASCII.SupersetConversion (StringSupersetConversion)
import Data.Kind (Type)

data ASCII (superset :: Type)

lift :: forall (superset :: Type). ASCII superset -> superset

asciiUnsafe :: forall (superset :: Type). superset -> ASCII superset

convertRefinedString :: StringSupersetConversion (a :: Type) (b :: Type) =>
  ASCII a -> ASCII b
