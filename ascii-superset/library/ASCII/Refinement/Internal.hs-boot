module ASCII.Refinement.Internal where

data ASCII superset

lift :: ASCII superset -> superset

asciiUnsafe :: superset -> ASCII superset
