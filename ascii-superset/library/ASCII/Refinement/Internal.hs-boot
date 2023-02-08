module ASCII.Refinement.Internal where

import {-# source #-} ASCII.SupersetConversion (StringSupersetConversion)

data ASCII superset

lift :: ASCII superset -> superset

asciiUnsafe :: superset -> ASCII superset

convertRefinedString :: StringSupersetConversion a b => ASCII a -> ASCII b
