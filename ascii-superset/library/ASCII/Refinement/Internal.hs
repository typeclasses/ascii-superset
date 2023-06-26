module ASCII.Refinement.Internal where

import ASCII.Char qualified as ASCII
import ASCII.Isomorphism qualified as I
import ASCII.Superset qualified as S
import {-# SOURCE #-} ASCII.SupersetConversion (StringSupersetConversion)
import {-# SOURCE #-} ASCII.SupersetConversion qualified as SupersetConversion
import Data.Bool qualified as Bool
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function (id, ($), (.))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List (map)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid)
import Data.Ord (Ord, (>))
import Data.Semigroup (Semigroup)
import GHC.Generics (Generic)
import Text.Show qualified as Show
import Prelude (succ)

-- | This type constructor indicates that a value from some ASCII superset
--    is valid ASCII
--
-- The type parameter is the ASCII superset, which should be a type with an
-- instance of either 'CharSuperset' or 'StringSuperset'.
--
-- For example, whereas a 'Data.Text.Text' value may contain a combination of ASCII
-- and non-ASCII characters, a value of type @'ASCII' 'Data.Text.Text'@ may contain
-- only ASCII characters.
newtype ASCII (superset :: Type) = ASCII_Unsafe {lift :: superset}

deriving stock instance Eq superset => Eq (ASCII superset)

deriving stock instance Ord superset => Ord (ASCII superset)

deriving newtype instance Hashable superset => Hashable (ASCII superset)

deriving newtype instance Semigroup superset => Semigroup (ASCII superset)

deriving newtype instance Monoid superset => Monoid (ASCII superset)

deriving stock instance Data superset => Data (ASCII superset)

deriving stock instance Generic (ASCII superset)

instance Show.Show superset => Show.Show (ASCII superset) where
  showsPrec d x =
    Show.showParen (d > app_prec) $
      Show.showString "asciiUnsafe " . Show.showsPrec (succ app_prec) (lift x)
    where
      app_prec = 10

  showList x = Show.showString "asciiUnsafe " . Show.showList (map lift x)

instance S.ToCaselessChar char => S.ToCaselessChar (ASCII char) where
  isAsciiCaselessChar _ = Bool.True
  toCaselessCharUnsafe = S.toCaselessCharUnsafe . lift

instance S.CharSuperset char => S.ToChar (ASCII char) where
  isAsciiChar _ = Bool.True
  toCharUnsafe = S.toCharUnsafe . lift

instance S.CharSuperset char => S.FromChar (ASCII char) where
  fromChar = asciiUnsafe . S.fromChar

instance S.CharSuperset char => S.CharSuperset (ASCII char) where
  toCaseChar c = asciiUnsafe . S.toCaseChar c . lift

instance S.CharSuperset char => I.CharIso (ASCII char) where
  toChar = S.toCharUnsafe

instance S.ToCaselessString string => S.ToCaselessString (ASCII string) where
  isAsciiCaselessString _ = Bool.True
  toCaselessCharListUnsafe = S.toCaselessCharListUnsafe . lift
  toCaselessCharListSub = S.toCaselessCharListSub . lift

instance S.ToString string => S.ToString (ASCII string) where
  isAsciiString _ = Bool.True
  toCharListUnsafe = S.toCharListUnsafe . lift
  toCharListSub = S.toCharListUnsafe . lift

instance S.FromString string => S.FromString (ASCII string) where
  fromCharList = asciiUnsafe . S.fromCharList

instance S.StringSuperset string => S.StringSuperset (ASCII string) where
  substituteString = id
  toCaseString c = asciiUnsafe . S.toCaseString c . lift

instance S.StringSuperset string => I.StringIso (ASCII string) where
  toCharList = S.toCharListUnsafe
  mapChars = S.mapCharsUnsafe

-- | Change the type of an ASCII superset value that is known to be valid ASCII
--
-- This is "unsafe" because this assertion is unchecked, so this function is capable
-- of producing an invalid 'ASCII' value.
asciiUnsafe :: superset -> ASCII superset
asciiUnsafe = ASCII_Unsafe

-- |
--
-- @
-- (map validateChar [-1, 65, 97, 128] :: [Maybe (ASCII Int)])
--     == [Nothing, Just (asciiUnsafe 65), Just (asciiUnsafe 97), Nothing]
-- @
validateChar :: S.CharSuperset superset => superset -> Maybe (ASCII superset)
validateChar x = if S.isAsciiChar x then Just (asciiUnsafe x) else Nothing

substituteChar :: S.CharSuperset superset => superset -> ASCII superset
substituteChar x = if S.isAsciiChar x then asciiUnsafe x else fromChar ASCII.Substitute

fromChar :: S.CharSuperset superset => ASCII.Char -> ASCII superset
fromChar = asciiUnsafe . S.fromChar

toChar :: S.CharSuperset superset => ASCII superset -> ASCII.Char
toChar = S.toCharUnsafe . lift

-- |
--
-- @
-- fromCharList [CapitalLetterH, SmallLetterI, ExclamationMark]
--     == (asciiUnsafe "Hi!" :: ASCII Text)
-- @
fromCharList :: S.StringSuperset superset => [ASCII.Char] -> ASCII superset
fromCharList = asciiUnsafe . S.fromCharList

-- |
--
-- @
-- toCharList (substituteString \"Piñata" :: ASCII Text) ==
--     [CapitalLetterP, SmallLetterI, Substitute, SmallLetterA, SmallLetterT, SmallLetterA]
-- @
toCharList :: S.StringSuperset superset => ASCII superset -> [ASCII.Char]
toCharList = S.toCharListUnsafe . lift

-- | Forces a string from a larger character set into ASCII by using the
--    'ASCII.Substitute' character in place of any non-ASCII characters
--
-- @
-- (substituteString \"Cristóbal" :: ASCII Text) == asciiUnsafe "Crist\SUBbal"
-- @
substituteString :: S.StringSuperset superset => superset -> ASCII superset
substituteString = asciiUnsafe . S.substituteString

-- |
--
-- @
-- (map validateString [\"Hello", \"Cristóbal"] :: [Maybe (ASCII Text)])
--     == [Just (asciiUnsafe \"Hello"), Nothing]
--
-- (map validateString [\"Hello", \"Cristóbal"] :: [Maybe (ASCII String)])
--     == [Just (asciiUnsafe \"Hello"), Nothing]
-- @
validateString :: S.StringSuperset superset => superset -> Maybe (ASCII superset)
validateString x = if S.isAsciiString x then Just (asciiUnsafe x) else Nothing

asChar :: S.CharSuperset superset => (ASCII.Char -> ASCII.Char) -> ASCII superset -> ASCII superset
asChar f = asciiUnsafe . S.asCharUnsafe f . lift

mapChars :: S.StringSuperset superset => (ASCII.Char -> ASCII.Char) -> ASCII superset -> ASCII superset
mapChars f = asciiUnsafe . S.mapCharsUnsafe f . lift

-- | For example, this function can convert @ASCII ByteString@ to @ASCII Text@ and vice versa
convertRefinedString :: StringSupersetConversion a b => ASCII a -> ASCII b
convertRefinedString (ASCII_Unsafe x) = ASCII_Unsafe (SupersetConversion.convertStringUnsafe x)
