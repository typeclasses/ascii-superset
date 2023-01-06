module ASCII.CaseRefinement where

import ASCII.Case (Case (..))

class KnownCase (letterCase :: Case) where theCase :: Case

instance KnownCase 'UpperCase
instance KnownCase 'LowerCase
