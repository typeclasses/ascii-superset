module ASCII.Superset
  ( -- * Characters

    -- ** Class
    ToCaselessChar (..),
    ToChar (..),
    FromChar (..),
    CharSuperset (..),
    ToCasefulChar (..),

    -- ** Functions
    asCharUnsafe,
    toCharMaybe,
    toCaselessCharMaybe,
    toCharOrFail,
    toCaselessCharOrFail,
    toCharSub,
    toCaselessCharSub,
    substituteChar,
    convertCharMaybe,
    convertCharOrFail,

    -- * Strings

    -- ** Class
    ToCaselessString (..),
    ToString (..),
    FromString (..),
    StringSuperset (..),
    ToCasefulString (..),

    -- ** Functions
    toCharListMaybe,
    toCaselessCharListMaybe,
    toCharListOrFail,
    toCaselessCharListOrFail,
    convertStringMaybe,
    convertStringOrFail,
  )
where

import ASCII.Case (Case (..))
import ASCII.Case qualified as Case
import {-# SOURCE #-} ASCII.CaseRefinement (KnownCase (..))
import ASCII.Caseless (CaselessChar)
import ASCII.Caseless qualified as Caseless
import ASCII.Char qualified as ASCII
import Control.Monad (return)
import Control.Monad.Fail (MonadFail (fail))
import Data.Bool (Bool, (&&))
import Data.Bool qualified as Bool
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as Unicode
import Data.Function (id, (.))
import Data.Functor (fmap)
import Data.Int qualified as Int
import Data.Kind (Type)
import Data.List qualified as List
import Data.Maybe (Maybe (..))
import Data.Ord ((<=), (>=))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as TB
import Data.Word qualified as Word
import Numeric.Natural qualified as Nat
import Prelude ((+), (-))
import Prelude qualified

---  Char  ---

-- | Partial conversion to 'CaselessChar'
--
-- Generally this will be a superset of the ASCII character set with a 'ToChar'
-- instance as well, and the conversion will be achieved by discarding the case of
-- letters. A notable exception is the instance for the 'CaselessChar' type itself,
-- which is already represented without case and does not have a 'ToChar' instance.
class ToCaselessChar (char :: Type) where
  -- | Test whether a character can be converted to 'CaselessChar'
  isAsciiCaselessChar :: char -> Bool

  -- | Conversion to 'CaselessChar', defined only where 'isAsciiCaselessChar' is satisfied
  toCaselessCharUnsafe :: char -> CaselessChar

-- | Partial conversion to 'ASCII.Char'
--
-- This includes the 'ASCII.Char' type itself, character sets that are supersets
-- of ASCII, and numeric types such as 'Word8' that are often used to represent
-- ASCII characters.
--
-- This does /not/ include 'CaselessChar', because that cannot be converted to
-- 'ASCII.Char' without choosing a case.
class ToCaselessChar char => ToChar (char :: Type) where
  -- | Test whether a character can be converted to 'ASCII.Char'
  isAsciiChar :: char -> Bool

  -- | Conversion to 'ASCII.Char', defined only where 'isAsciiChar' is satisfied
  toCharUnsafe :: char -> ASCII.Char

-- | Total conversion from 'ASCII.Char'
--
-- This class includes supersets of ASCII, in which case 'fromChar' is a lifting
-- function. It also includes 'CaselessChar', in which case 'fromChar' discards
-- case information.
--
-- This does /not/ include 'ASCII.CaseRefinement.ASCII'case', because that represents
-- a subset of 'ASCII.Char'; not all characters are of the wanted case, so no total
-- conversion is possible without changing case.
class FromChar (char :: Type) where
  -- | Conversion from 'ASCII.Char'
  fromChar :: ASCII.Char -> char

-- | Character type with:
--
-- - a total conversion from ASCII; and
-- - a partial conversion to ASCII
class (ToChar char, FromChar char) => CharSuperset (char :: Type) where
  -- | Convert a character in the superset to the designated case,
  --    if it is an ASCII letter of the opposite case. Otherwise, return
  --    the argument unmodified.
  toCaseChar :: Case -> char -> char

class ToCasefulChar (letterCase :: Case) (char :: Type) where
  toCasefulChar :: CaselessChar -> char

-- | Manipulate a character as if it were an ASCII 'ASCII.Char', assuming that it is
--
-- Defined only where 'isAsciiChar' is satisfied.
asCharUnsafe :: CharSuperset char => (ASCII.Char -> ASCII.Char) -> char -> char
asCharUnsafe f = fromChar . f . toCharUnsafe

toCharMaybe :: ToChar char => char -> Maybe ASCII.Char
toCharMaybe = toCharOrFail

toCaselessCharMaybe :: ToCaselessChar char => char -> Maybe CaselessChar
toCaselessCharMaybe = toCaselessCharOrFail

toCharOrFail :: (ToChar char, MonadFail context) => char -> context ASCII.Char
toCharOrFail x =
  if isAsciiChar x
    then return (toCharUnsafe x)
    else fail "Not an ASCII character"

toCaselessCharOrFail :: (ToCaselessChar char, MonadFail context) => char -> context CaselessChar
toCaselessCharOrFail x =
  if isAsciiCaselessChar x
    then return (toCaselessCharUnsafe x)
    else fail "Not an ASCII character"

toCharSub :: ToChar char => char -> ASCII.Char
toCharSub x = if isAsciiChar x then toCharUnsafe x else ASCII.Substitute

toCaselessCharSub :: ToCaselessChar char => char -> CaselessChar
toCaselessCharSub x = if isAsciiCaselessChar x then toCaselessCharUnsafe x else Caseless.Substitute

-- | Force a character into ASCII by replacing it with 'ASCII.Substitute' if it
--    is not already an ASCII character
--
-- The resulting character satisfies 'isAsciiChar' and 'isAsciiCaselessChar'.
substituteChar :: CharSuperset char => char -> char
substituteChar x = if isAsciiChar x then x else fromChar ASCII.Substitute

-- | Convert from one ASCII-superset character type to another via the ASCII
-- 'ASCII.Char' type. Fails as 'Nothing' if the input is outside the ASCII
-- character set.
convertCharMaybe :: (ToChar char1, FromChar char2) => char1 -> Maybe char2
convertCharMaybe = convertCharOrFail

-- | Convert from one ASCII-superset character type to another via the ASCII
-- 'ASCII.Char' type. Fails with 'fail' if the input is outside the ASCII character
-- set.
convertCharOrFail ::
  (ToChar char1, FromChar char2, MonadFail context) =>
  char1 ->
  context char2
convertCharOrFail = fmap fromChar . toCharOrFail

---  String  ---

-- | Partial conversion to @['CaselessChar']@
--
-- Generally this will be a superset of ASCII strings with a 'ToString' instance as
-- well, and the conversion will be achieved by discarding the case of letters. A
-- notable exception is the instance for @['CaselessChar']@ type itself, which is
-- already represented without case and does not have a 'ToString' instance.
class ToCaselessString (string :: Type) where
  -- | Test whether a character can be converted to @['CaselessChar']@
  isAsciiCaselessString :: string -> Bool

  -- | Conversion to @['CaselessChar']@, defined only where
  --        'isAsciiCaselessString' is satisfied
  toCaselessCharListUnsafe :: string -> [CaselessChar]

  -- | Conversion to @['CaselessChar']@ achieved by using
  --        'Caseless.Substitute' in place of any non-ASCII characters
  toCaselessCharListSub :: string -> [CaselessChar]

-- | Partial conversion to @['ASCII.Char']@
--
-- This includes @['ASCII.Char']@ type itself, strings of character sets that are
-- supersets of ASCII, and sequences of numeric types such as 'Word8' that are
-- often used to represent ASCII characters.
--
-- This does /not/ include @['CaselessChar']@, because that cannot be converted
-- to @['ASCII.Char']@ without choosing a case.
class ToCaselessString string => ToString (string :: Type) where
  -- | Test whether a string can be converted to @['ASCII.Char']@
  isAsciiString :: string -> Bool

  -- | Conversion to @['ASCII.Char']@, defined only where 'isAsciiString'
  --        is satisfied
  toCharListUnsafe :: string -> [ASCII.Char]

  -- | Conversion to @['ASCII.Char']@ achieved by using
  --        'ASCII.Substitute' in place of any non-ASCII characters
  toCharListSub :: string -> [ASCII.Char]

-- | Total conversion from @['ASCII.Char']@
--
-- This class includes supersets of ASCII, in which case 'fromCharList' lifts each
-- character into the larger character set. It also includes @['CaselessChar']@, in
-- which case 'fromCharList' discards case information from letters.
--
-- This does /not/ include @['ASCII.CaseRefinement.ASCII'case']@, because that
-- represents a subset of ASCII; not all ASCII characters are of case wanted by
-- 'ASCII.CaseRefinement.ASCII'case', so no total conversion is possible without
-- changing case.
class FromString (string :: Type) where
  -- | Conversion from @['ASCII.Char']@
  fromCharList :: [ASCII.Char] -> string

-- | String type with:
--
-- - a total conversion from ASCII; and
-- - a partial conversion to ASCII
class (ToString string, FromString string) => StringSuperset (string :: Type) where
  -- | Force a string into ASCII by replacing any non-ASCII character with 'ASCII.Substitute'
  --
  --        The resulting string satisfies 'isAsciiString' and 'isAsciiCaselessString'.
  substituteString :: string -> string

  mapCharsUnsafe :: (ASCII.Char -> ASCII.Char) -> string -> string
  mapCharsUnsafe f = fromCharList . List.map f . toCharListUnsafe

  -- | Convert each character in the superset to the designated case, if it is
  --    an ASCII letter of the opposite case. Leaves other characters unchanged.
  toCaseString :: Case -> string -> string

class ToCasefulString (letterCase :: Case) (string :: Type) where
  toCasefulString :: [CaselessChar] -> string

toCharListMaybe :: ToString string => string -> Maybe [ASCII.Char]
toCharListMaybe = toCharListOrFail

toCaselessCharListMaybe :: ToCaselessString string => string -> Maybe [CaselessChar]
toCaselessCharListMaybe = toCaselessCharListOrFail

toCharListOrFail ::
  (ToString string, MonadFail context) =>
  string ->
  context [ASCII.Char]
toCharListOrFail x =
  if isAsciiString x
    then return (toCharListUnsafe x)
    else fail "String contains non-ASCII characters"

toCaselessCharListOrFail ::
  (ToCaselessString string, MonadFail context) =>
  string ->
  context [CaselessChar]
toCaselessCharListOrFail x =
  if isAsciiCaselessString x
    then return (toCaselessCharListUnsafe x)
    else fail "String contains non-ASCII characters"

-- | Convert from one ASCII-superset string type to another by converting each
-- character of the input string to an ASCII 'ASCII.Char', and then converting the
-- ASCII character list to the desired output type. Fails as 'Nothing' if the input
-- contains any character that is outside the ASCII character set.
convertStringMaybe ::
  (ToString string1, FromString string2) =>
  string1 ->
  Maybe string2
convertStringMaybe = convertStringOrFail

-- | Convert from one ASCII-superset string type to another by converting each
-- character of the input string to an ASCII 'ASCII.Char', and then converting the
-- ASCII character list to the desired output type. Fails with 'fail' if the input
-- contains any character that is outside the ASCII character set.
convertStringOrFail ::
  (ToString string1, FromString string2, MonadFail context) =>
  string1 ->
  context string2
convertStringOrFail = fmap fromCharList . toCharListOrFail

---  Instances  ---

-- | 'CaselessChar' is trivially convertible to itself.
instance ToCaselessChar CaselessChar where
  isAsciiCaselessChar _ = Bool.True
  toCaselessCharUnsafe = id

instance FromChar CaselessChar where
  fromChar = Caseless.disregardCase

---

instance ToCaselessChar ASCII.Char where
  isAsciiCaselessChar _ = Bool.True
  toCaselessCharUnsafe = Caseless.disregardCase

instance ToChar ASCII.Char where
  isAsciiChar _ = Bool.True
  toCharUnsafe = id

instance FromChar ASCII.Char where
  fromChar = id

-- | 'ASCII.Char' is trivially a superset of itself.
instance CharSuperset ASCII.Char where
  toCaseChar = Case.toCase

instance KnownCase letterCase => ToCasefulChar letterCase ASCII.Char where
  toCasefulChar = Caseless.toCase (theCase @letterCase)

---

instance ToCaselessChar Unicode.Char where
  isAsciiCaselessChar = isAsciiChar
  toCaselessCharUnsafe = toCaselessCharUnsafe . toCharUnsafe

instance ToChar Unicode.Char where
  isAsciiChar = (<= '\DEL')
  toCharUnsafe = toCharUnsafe @Int.Int . Unicode.ord

instance FromChar Unicode.Char where
  fromChar = Unicode.chr . ASCII.toInt

instance CharSuperset Unicode.Char where
  toCaseChar UpperCase x | x >= 'a' && x <= 'z' = Unicode.chr (Unicode.ord x - 32)
  toCaseChar LowerCase x | x >= 'A' && x <= 'Z' = Unicode.chr (Unicode.ord x + 32)
  toCaseChar _ x = x

instance KnownCase letterCase => ToCasefulChar letterCase Unicode.Char where
  toCasefulChar = fromChar . toCasefulChar @letterCase

---

instance ToCaselessChar Nat.Natural where
  isAsciiCaselessChar = isAsciiChar
  toCaselessCharUnsafe = toCaselessCharUnsafe . toCharUnsafe

instance ToChar Nat.Natural where
  isAsciiChar = (<= 127)
  toCharUnsafe = toCharUnsafe @Int.Int . Prelude.fromIntegral

instance FromChar Nat.Natural where
  fromChar = Prelude.fromIntegral . ASCII.toInt

instance CharSuperset Nat.Natural where
  toCaseChar UpperCase x | x >= 97 && x <= 122 = x - 32
  toCaseChar LowerCase x | x >= 65 && x <= 90 = x + 32
  toCaseChar _ x = x

instance KnownCase letterCase => ToCasefulChar letterCase Nat.Natural where
  toCasefulChar = fromChar . toCasefulChar @letterCase

---

instance ToCaselessChar Int.Int where
  isAsciiCaselessChar = isAsciiChar
  toCaselessCharUnsafe = toCaselessCharUnsafe . toCharUnsafe

instance ToChar Int.Int where
  isAsciiChar x = (x >= 0) && (x <= 127)
  toCharUnsafe = ASCII.fromIntUnsafe

instance FromChar Int.Int where
  fromChar = ASCII.toInt

instance CharSuperset Int.Int where
  toCaseChar UpperCase x | x >= 97 && x <= 122 = x - 32
  toCaseChar LowerCase x | x >= 65 && x <= 90 = x + 32
  toCaseChar _ x = x

instance KnownCase letterCase => ToCasefulChar letterCase Int.Int where
  toCasefulChar = fromChar . toCasefulChar @letterCase

---

instance ToCaselessChar Word.Word8 where
  isAsciiCaselessChar = isAsciiChar
  toCaselessCharUnsafe = toCaselessCharUnsafe . toCharUnsafe

instance ToChar Word.Word8 where
  isAsciiChar = (<= 127)
  toCharUnsafe = ASCII.fromIntUnsafe . Prelude.fromIntegral

instance FromChar Word.Word8 where
  fromChar = Prelude.fromIntegral . ASCII.toInt

instance CharSuperset Word.Word8 where
  toCaseChar UpperCase x | x >= 97 && x <= 122 = x - 32
  toCaseChar LowerCase x | x >= 65 && x <= 90 = x + 32
  toCaseChar _ x = x

instance KnownCase letterCase => ToCasefulChar letterCase Word.Word8 where
  toCasefulChar = fromChar . toCasefulChar @letterCase

---

instance ToCaselessChar char => ToCaselessString [char] where
  isAsciiCaselessString = List.all isAsciiCaselessChar
  toCaselessCharListUnsafe = List.map toCaselessCharUnsafe
  toCaselessCharListSub = List.map toCaselessCharSub

instance ToChar char => ToString [char] where
  isAsciiString = List.all isAsciiChar
  toCharListUnsafe = List.map toCharUnsafe
  toCharListSub = List.map toCharSub

instance FromChar char => FromString [char] where
  fromCharList = List.map fromChar

instance CharSuperset char => StringSuperset [char] where
  substituteString = List.map substituteChar
  toCaseString c = List.map (toCaseChar c)

instance (ToCasefulChar letterCase char, KnownCase letterCase) => ToCasefulString letterCase [char] where
  toCasefulString = List.map (toCasefulChar @letterCase)

---

instance ToCaselessString T.Text where
  isAsciiCaselessString = T.all isAsciiChar
  toCaselessCharListUnsafe = toCaselessCharListUnsafe . T.unpack
  toCaselessCharListSub = toCaselessCharListSub . T.unpack

instance ToString T.Text where
  isAsciiString = T.all isAsciiChar
  toCharListUnsafe = toCharListUnsafe . T.unpack
  toCharListSub = toCharListSub . T.unpack

instance FromString T.Text where
  fromCharList = T.pack . fromCharList

instance StringSuperset T.Text where
  substituteString = T.map substituteChar
  mapCharsUnsafe f = T.map (asCharUnsafe f)
  toCaseString c = T.map (toCaseChar c)

instance KnownCase letterCase => ToCasefulString letterCase T.Text where
  toCasefulString = fromCharList . toCasefulString @letterCase

---

instance ToCaselessString LT.Text where
  isAsciiCaselessString = LT.all isAsciiChar
  toCaselessCharListUnsafe = toCaselessCharListUnsafe . LT.unpack
  toCaselessCharListSub = toCaselessCharListSub . LT.unpack

instance ToString LT.Text where
  isAsciiString = LT.all isAsciiChar
  toCharListUnsafe = toCharListUnsafe . LT.unpack
  toCharListSub = toCharListSub . LT.unpack

instance FromString LT.Text where
  fromCharList = LT.pack . fromCharList

instance StringSuperset LT.Text where
  substituteString = LT.map substituteChar
  mapCharsUnsafe f = LT.map (asCharUnsafe f)
  toCaseString c = LT.map (toCaseChar c)

instance KnownCase letterCase => ToCasefulString letterCase LT.Text where
  toCasefulString = fromCharList . toCasefulString @letterCase

---

instance ToCaselessString TB.Builder where
  isAsciiCaselessString = isAsciiCaselessString . TB.toLazyText
  toCaselessCharListUnsafe = toCaselessCharListUnsafe . TB.toLazyText
  toCaselessCharListSub = toCaselessCharListSub . TB.toLazyText

instance ToString TB.Builder where
  isAsciiString = isAsciiString . TB.toLazyText
  toCharListUnsafe = toCharListUnsafe . TB.toLazyText
  toCharListSub = toCharListSub . TB.toLazyText

instance FromString TB.Builder where
  fromCharList = TB.fromString . fromCharList

instance StringSuperset TB.Builder where
  substituteString = TB.fromLazyText . substituteString . TB.toLazyText
  mapCharsUnsafe f = TB.fromLazyText . mapCharsUnsafe f . TB.toLazyText
  toCaseString c = TB.fromLazyText . toCaseString c . TB.toLazyText

instance KnownCase letterCase => ToCasefulString letterCase TB.Builder where
  toCasefulString = fromCharList . toCasefulString @letterCase

---

instance ToCaselessString BS.ByteString where
  isAsciiCaselessString = BS.all isAsciiCaselessChar
  toCaselessCharListUnsafe = toCaselessCharListUnsafe . BS.unpack
  toCaselessCharListSub = toCaselessCharListSub . BS.unpack

instance ToString BS.ByteString where
  isAsciiString = BS.all isAsciiChar
  toCharListUnsafe = toCharListUnsafe . BS.unpack
  toCharListSub = toCharListSub . BS.unpack

instance FromString BS.ByteString where
  fromCharList = BS.pack . fromCharList

instance StringSuperset BS.ByteString where
  substituteString = BS.map substituteChar
  mapCharsUnsafe f = BS.map (asCharUnsafe f)
  toCaseString c = BS.map (toCaseChar c)

instance KnownCase letterCase => ToCasefulString letterCase BS.ByteString where
  toCasefulString = fromCharList . toCasefulString @letterCase

---

instance ToCaselessString LBS.ByteString where
  isAsciiCaselessString = LBS.all isAsciiCaselessChar
  toCaselessCharListUnsafe = toCaselessCharListUnsafe . LBS.unpack
  toCaselessCharListSub = toCaselessCharListSub . LBS.unpack

instance ToString LBS.ByteString where
  isAsciiString = LBS.all isAsciiChar
  toCharListUnsafe = toCharListUnsafe . LBS.unpack
  toCharListSub = toCharListSub . LBS.unpack

instance FromString LBS.ByteString where
  fromCharList = LBS.pack . fromCharList

instance StringSuperset LBS.ByteString where
  substituteString = LBS.map substituteChar
  mapCharsUnsafe f = LBS.map (asCharUnsafe f)
  toCaseString c = LBS.map (toCaseChar c)

instance KnownCase letterCase => ToCasefulString letterCase LBS.ByteString where
  toCasefulString = fromCharList . toCasefulString @letterCase

---

instance ToCaselessString BSB.Builder where
  isAsciiCaselessString = isAsciiCaselessString . BSB.toLazyByteString
  toCaselessCharListUnsafe = toCaselessCharListUnsafe . BSB.toLazyByteString
  toCaselessCharListSub = toCaselessCharListSub . BSB.toLazyByteString

instance ToString BSB.Builder where
  isAsciiString = isAsciiString . BSB.toLazyByteString
  toCharListUnsafe = toCharListUnsafe . BSB.toLazyByteString
  toCharListSub = toCharListSub . BSB.toLazyByteString

instance FromString BSB.Builder where
  fromCharList = BSB.lazyByteString . fromCharList

instance StringSuperset BSB.Builder where
  substituteString = BSB.lazyByteString . substituteString . BSB.toLazyByteString
  mapCharsUnsafe f = BSB.lazyByteString . mapCharsUnsafe f . BSB.toLazyByteString
  toCaseString c = BSB.lazyByteString . toCaseString c . BSB.toLazyByteString

instance KnownCase letterCase => ToCasefulString letterCase BSB.Builder where
  toCasefulString = fromCharList . toCasefulString @letterCase
