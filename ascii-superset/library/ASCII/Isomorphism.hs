module ASCII.Isomorphism (CharIso (..), asChar, StringIso (..)) where

import ASCII.Char (Char)
import ASCII.Superset (CharSuperset, StringSuperset, fromChar)
import Data.Function (id, (.))
import Data.Kind (Type)
import Data.List (map)

class CharSuperset (char :: Type) => CharIso char where
  toChar :: char -> Char

asChar :: CharIso char => (Char -> Char) -> char -> char
asChar f = fromChar . f . toChar

class StringSuperset string => StringIso (string :: Type) where
  toCharList :: string -> [Char]
  mapChars :: (Char -> Char) -> string -> string

-- | 'Char' is trivially isomorphic to itself. (This instance is uninteresting.)
instance CharIso Char where
  toChar = id

instance CharIso char => StringIso [char] where
  toCharList = map toChar
  mapChars f = map (asChar f)
