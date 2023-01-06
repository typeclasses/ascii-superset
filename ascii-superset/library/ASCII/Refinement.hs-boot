module ASCII.Refinement where

data ASCII superset

lift :: ASCII superset -> superset

asciiUnsafe :: superset -> ASCII superset
