module ASCII.Refinement
  ( -- * ASCII type constructor
    ASCII,
    lift,
    asciiUnsafe,

    -- * Character functions
    validateChar,
    fromChar,
    toChar,
    substituteChar,
    asChar,

    -- * String functions
    validateString,
    fromCharList,
    toCharList,
    substituteString,
    mapChars,
  )
where

import ASCII.Refinement.Internal
