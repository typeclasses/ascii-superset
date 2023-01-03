module ASCII.Superset
  (
    {- * Characters -}
    {- ** Class -} ToCaselessChar (..), ToChar (..), FromChar (..), CharSuperset,
    {- ** Functions -} asCharUnsafe, toCharMaybe, toCaselessCharMaybe, toCharOrFail, toCaselessCharOrFail,
        toCharSub, toCaselessCharSub, substituteChar, convertCharMaybe, convertCharOrFail,

    {- * Strings -}
    {- ** Class -} ToCaselessString (..), ToString (..), FromString (..), StringSuperset (..),
    {- ** Functions -} toCharListMaybe, toCaselessCharListMaybe, toCharListOrFail, toCaselessCharListOrFail,
        convertStringMaybe, convertStringOrFail
  )
  where

import ASCII.Caseless (CaselessChar)
import Control.Monad (return)
import Control.Monad.Fail (MonadFail (fail))
import Data.Bool (Bool, (&&))
import Data.Function (id, (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe (..))
import Data.Ord ((<=), (>=))

import qualified ASCII.Caseless as Caseless
import qualified ASCII.Char as ASCII
import qualified Data.Bool as Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Unicode
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Word as Word
import qualified Numeric.Natural as Nat
import qualified Prelude


---  Char  ---

class ToCaselessChar char where

    isAsciiCaselessChar :: char -> Bool

    toCaselessCharUnsafe :: char -> CaselessChar

class ToCaselessChar char => ToChar char where

    isAsciiChar :: char -> Bool

    toCharUnsafe :: char -> ASCII.Char

class FromChar char where

    fromChar :: ASCII.Char -> char

class (ToChar char, FromChar char) => CharSuperset char

asCharUnsafe :: CharSuperset char => (ASCII.Char -> ASCII.Char) -> char -> char
asCharUnsafe f = fromChar . f . toCharUnsafe

toCharMaybe :: ToChar char => char -> Maybe ASCII.Char
toCharMaybe = toCharOrFail

toCaselessCharMaybe :: ToCaselessChar char => char -> Maybe CaselessChar
toCaselessCharMaybe = toCaselessCharOrFail

toCharOrFail :: (ToChar char, MonadFail context) => char -> context ASCII.Char
toCharOrFail x = if isAsciiChar x then return (toCharUnsafe x)
    else fail "Not an ASCII character"

toCaselessCharOrFail :: (ToCaselessChar char, MonadFail context) => char -> context CaselessChar
toCaselessCharOrFail x = if isAsciiCaselessChar x then return (toCaselessCharUnsafe x)
    else fail "Not an ASCII character"

toCharSub :: ToChar char => char -> ASCII.Char
toCharSub x = if isAsciiChar x then toCharUnsafe x else ASCII.Substitute

toCaselessCharSub :: ToCaselessChar char => char -> CaselessChar
toCaselessCharSub x = if isAsciiCaselessChar x then toCaselessCharUnsafe x else Caseless.Substitute

substituteChar :: CharSuperset char => char -> char
substituteChar x = if isAsciiChar x then x else fromChar ASCII.Substitute

{-| Convert from one ASCII-superset character type to another via the ASCII
'ASCII.Char' type. Fails as 'Nothing' if the input is outside the ASCII
character set. -}
convertCharMaybe :: (ToChar char1, FromChar char2) =>
    char1 -> Maybe char2
convertCharMaybe = convertCharOrFail

{-| Convert from one ASCII-superset character type to another via the ASCII
'ASCII.Char' type. Fails with 'fail' if the input is outside the ASCII character
set. -}
convertCharOrFail :: (ToChar char1, FromChar char2, MonadFail context) =>
    char1 -> context char2
convertCharOrFail = fmap fromChar . toCharOrFail


---  String  ---

class ToCaselessString string where

    isAsciiCaselessString :: string -> Bool

    toCaselessCharListUnsafe :: string -> [CaselessChar]

    toCaselessCharListSub :: string -> [CaselessChar]

class ToCaselessString string => ToString string where

    isAsciiString :: string -> Bool

    toCharListUnsafe :: string -> [ASCII.Char]

    toCharListSub :: string -> [ASCII.Char]

class FromString string where

    fromCharList :: [ASCII.Char] -> string

class (ToString string, FromString string) => StringSuperset string where

    substituteString :: string -> string

    mapCharsUnsafe :: (ASCII.Char -> ASCII.Char) -> string -> string
    mapCharsUnsafe f = fromCharList  . List.map f . toCharListUnsafe

toCharListMaybe :: ToString string => string -> Maybe [ASCII.Char]
toCharListMaybe = toCharListOrFail

toCaselessCharListMaybe :: ToCaselessString string => string -> Maybe [CaselessChar]
toCaselessCharListMaybe = toCaselessCharListOrFail

toCharListOrFail :: (ToString string, MonadFail context) =>
    string -> context [ASCII.Char]
toCharListOrFail x = if isAsciiString x then return (toCharListUnsafe x)
    else fail "String contains non-ASCII characters"

toCaselessCharListOrFail :: (ToCaselessString string, MonadFail context) =>
    string -> context [CaselessChar]
toCaselessCharListOrFail x = if isAsciiCaselessString x then return (toCaselessCharListUnsafe x)
    else fail "String contains non-ASCII characters"

{-| Convert from one ASCII-superset string type to another by converting each
character of the input string to an ASCII 'ASCII.Char', and then converting the
ASCII character list to the desired output type. Fails as 'Nothing' if the input
contains any character that is outside the ASCII character set. -}
convertStringMaybe :: (ToString string1, FromString string2) =>
    string1 -> Maybe string2
convertStringMaybe = convertStringOrFail

{-| Convert from one ASCII-superset string type to another by converting each
character of the input string to an ASCII 'ASCII.Char', and then converting the
ASCII character list to the desired output type. Fails with 'fail' if the input
contains any character that is outside the ASCII character set. -}
convertStringOrFail :: (ToString string1, FromString string2, MonadFail context) =>
    string1 -> context string2
convertStringOrFail = fmap fromCharList . toCharListOrFail


---  Instances  ---

-- | 'CaselessChar' is trivially convertible to itself. (This instance is uninteresting.)
instance ToCaselessChar CaselessChar where
    isAsciiCaselessChar _ = Bool.True
    toCaselessCharUnsafe = id

---

instance ToCaselessChar ASCII.Char where
    isAsciiCaselessChar _ = Bool.True
    toCaselessCharUnsafe = Caseless.disregardCase

instance ToChar ASCII.Char where
    isAsciiChar _ = Bool.True
    toCharUnsafe = id

instance FromChar ASCII.Char where
    fromChar = id

-- | 'ASCII.Char' is trivially a superset of itself. (This instance is uninteresting.)
instance CharSuperset ASCII.Char

---

instance ToCaselessChar Unicode.Char where
    isAsciiCaselessChar = isAsciiChar
    toCaselessCharUnsafe = toCaselessCharUnsafe . toCharUnsafe

instance ToChar Unicode.Char where
    isAsciiChar = (<= '\DEL')
    toCharUnsafe = toCharUnsafe @Int.Int . Unicode.ord

instance FromChar Unicode.Char where
    fromChar = Unicode.chr . ASCII.toInt

instance CharSuperset Unicode.Char

---

instance ToCaselessChar Nat.Natural where
    isAsciiCaselessChar = isAsciiChar
    toCaselessCharUnsafe = toCaselessCharUnsafe . toCharUnsafe

instance ToChar Nat.Natural where
    isAsciiChar = (<= 127)
    toCharUnsafe = toCharUnsafe @Int.Int . Prelude.fromIntegral

instance FromChar Nat.Natural where
    fromChar = Prelude.fromIntegral . ASCII.toInt

instance CharSuperset Nat.Natural

---

instance ToCaselessChar Int.Int where
    isAsciiCaselessChar = isAsciiChar
    toCaselessCharUnsafe = toCaselessCharUnsafe . toCharUnsafe

instance ToChar Int.Int where
    isAsciiChar x = (x >= 0) && (x <= 127)
    toCharUnsafe = ASCII.fromIntUnsafe

instance FromChar Int.Int where
    fromChar = ASCII.toInt

instance CharSuperset Int.Int

---

instance ToCaselessChar Word.Word8 where
    isAsciiCaselessChar = isAsciiChar
    toCaselessCharUnsafe = toCaselessCharUnsafe . toCharUnsafe

instance ToChar Word.Word8 where
    isAsciiChar = (<= 127)
    toCharUnsafe = ASCII.fromIntUnsafe . Prelude.fromIntegral

instance FromChar Word.Word8 where
    fromChar = Prelude.fromIntegral . ASCII.toInt

instance CharSuperset Word.Word8

---

instance ToCaselessChar char => ToCaselessString [char] where
    isAsciiCaselessString    = List.all isAsciiCaselessChar
    toCaselessCharListUnsafe = List.map toCaselessCharUnsafe
    toCaselessCharListSub    = List.map toCaselessCharSub

instance ToChar char => ToString [char] where
    isAsciiString    = List.all isAsciiChar
    toCharListUnsafe = List.map toCharUnsafe
    toCharListSub    = List.map toCharSub

instance FromChar char => FromString [char] where
    fromCharList = List.map fromChar

instance CharSuperset char => StringSuperset [char] where
    substituteString = List.map substituteChar

---

instance ToCaselessString T.Text where
    isAsciiCaselessString = T.all isAsciiChar
    toCaselessCharListUnsafe = toCaselessCharListUnsafe . T.unpack
    toCaselessCharListSub    = toCaselessCharListSub    . T.unpack

instance ToString T.Text where
    isAsciiString = T.all isAsciiChar
    toCharListUnsafe = toCharListUnsafe . T.unpack
    toCharListSub = toCharListSub . T.unpack

instance FromString T.Text where
    fromCharList = T.pack . fromCharList

instance StringSuperset T.Text where
    substituteString = T.map substituteChar
    mapCharsUnsafe f = T.map (asCharUnsafe f)

---

instance ToCaselessString LT.Text where
    isAsciiCaselessString = LT.all isAsciiChar
    toCaselessCharListUnsafe = toCaselessCharListUnsafe . LT.unpack
    toCaselessCharListSub    = toCaselessCharListSub    . LT.unpack

instance ToString LT.Text where
    isAsciiString = LT.all isAsciiChar
    toCharListUnsafe = toCharListUnsafe . LT.unpack
    toCharListSub    = toCharListSub    . LT.unpack

instance FromString LT.Text where
    fromCharList = LT.pack . fromCharList

instance StringSuperset LT.Text where
    substituteString = LT.map substituteChar
    mapCharsUnsafe f = LT.map (asCharUnsafe f)

---

instance ToCaselessString TB.Builder where
    isAsciiCaselessString    = isAsciiCaselessString    . TB.toLazyText
    toCaselessCharListUnsafe = toCaselessCharListUnsafe . TB.toLazyText
    toCaselessCharListSub    = toCaselessCharListSub    . TB.toLazyText

instance ToString TB.Builder where
    isAsciiString    = isAsciiString    . TB.toLazyText
    toCharListUnsafe = toCharListUnsafe . TB.toLazyText
    toCharListSub    = toCharListSub    . TB.toLazyText

instance FromString TB.Builder where
    fromCharList = TB.fromString . fromCharList

instance StringSuperset TB.Builder where
    substituteString = TB.fromLazyText . substituteString . TB.toLazyText
    mapCharsUnsafe f = TB.fromLazyText . mapCharsUnsafe f . TB.toLazyText

---

instance ToCaselessString BS.ByteString where
    isAsciiCaselessString = BS.all isAsciiCaselessChar
    toCaselessCharListUnsafe = toCaselessCharListUnsafe . BS.unpack
    toCaselessCharListSub    = toCaselessCharListSub    . BS.unpack

instance ToString BS.ByteString where
    isAsciiString = BS.all isAsciiChar
    toCharListUnsafe = toCharListUnsafe . BS.unpack
    toCharListSub    = toCharListSub    . BS.unpack

instance FromString BS.ByteString where
    fromCharList = BS.pack . fromCharList

instance StringSuperset BS.ByteString where
    substituteString = BS.map substituteChar
    mapCharsUnsafe f = BS.map (asCharUnsafe f)

---

instance ToCaselessString LBS.ByteString where
    isAsciiCaselessString = LBS.all isAsciiCaselessChar
    toCaselessCharListUnsafe = toCaselessCharListUnsafe . LBS.unpack
    toCaselessCharListSub    = toCaselessCharListSub    . LBS.unpack

instance ToString LBS.ByteString where
    isAsciiString = LBS.all isAsciiChar
    toCharListUnsafe = toCharListUnsafe . LBS.unpack
    toCharListSub    = toCharListSub    . LBS.unpack

instance FromString LBS.ByteString where
    fromCharList = LBS.pack . fromCharList

instance StringSuperset LBS.ByteString where
    substituteString = LBS.map substituteChar
    mapCharsUnsafe f = LBS.map (asCharUnsafe f)

---

instance ToCaselessString BSB.Builder where
    isAsciiCaselessString    = isAsciiCaselessString    . BSB.toLazyByteString
    toCaselessCharListUnsafe = toCaselessCharListUnsafe . BSB.toLazyByteString
    toCaselessCharListSub    = toCaselessCharListSub    . BSB.toLazyByteString

instance ToString BSB.Builder where
    isAsciiString    = isAsciiString    . BSB.toLazyByteString
    toCharListUnsafe = toCharListUnsafe . BSB.toLazyByteString
    toCharListSub    = toCharListSub    . BSB.toLazyByteString

instance FromString BSB.Builder where
    fromCharList = BSB.lazyByteString . fromCharList

instance StringSuperset BSB.Builder where
    substituteString = BSB.lazyByteString . substituteString . BSB.toLazyByteString
    mapCharsUnsafe f = BSB.lazyByteString . mapCharsUnsafe f . BSB.toLazyByteString
