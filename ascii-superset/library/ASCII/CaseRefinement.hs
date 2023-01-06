module ASCII.CaseRefinement
  (
    {- * ASCII'case type constructor -} ASCII'case, lift, asciiCaseUnsafe,
    {- ** Aliases -} {- $aliases -} ASCII'upper, ASCII'lower,
    {- * Character functions -} validateChar, fromCaselessChar,
          toCaselessChar, substituteChar, asCaselessChar, refineCharToCase,
    {- * String functions -} validateString, fromCaselessCharList,
          toCaselessCharList, substituteString, mapChars, refineStringToCase,
    {- * KnownCase -} KnownCase (..),
  )
  where

import ASCII.Case (Case (..))
import ASCII.Caseless (CaselessChar)
import {-# source #-} ASCII.Refinement (ASCII)
import ASCII.Superset (CharSuperset, StringSuperset)

import qualified ASCII.Case as Case
import qualified ASCII.Caseless as Caseless
import qualified ASCII.Char as ASCII
import qualified ASCII.Superset as Superset
import {-# source #-} qualified ASCII.Refinement as Refinement

import Control.Monad (guard)
import Data.Bool (Bool (..))
import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Foldable (any)
import Data.Function (id, ($), (.))
import Data.Hashable (Hashable)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid)
import Data.Ord (Ord, (>))
import Data.Semigroup (Semigroup)
import Data.Traversable (traverse)
import GHC.Generics (Generic)
import Prelude (succ)
import Text.Show (Show, showList, showParen, showString, showsPrec)

import qualified Data.Bool as Bool
import qualified Data.List as List

{-| Indicates that a value from some ASCII superset is valid ASCII, and also
    that any letters belong to a particular 'Case' indicated by the @letterCase@
    type parameter

The @superset@ type parameter is the ASCII superset, which should be a type with
an instance of either 'CharSuperset' or 'StringSuperset'.

For example, whereas a 'Data.Text.Text' value may contain a combination of ASCII
and non-ASCII characters, a value of type @'ASCII'case' ''ASCII.Case.UpperCase'
'Data.Text.Text'@ may contain only uppercase ASCII letters and ASCII
non-letters. -}
newtype ASCII'case (letterCase :: Case) superset = ASCII'case_Unsafe
  { lift :: superset
      {- ^ Discard the evidence that the value is known to consist
           entirely of ASCII characters in a particular case -}
  }

deriving stock instance Eq superset =>
    Eq (ASCII'case letterCase superset)

deriving stock instance Ord superset =>
    Ord (ASCII'case letterCase superset)

deriving newtype instance Hashable superset =>
    Hashable (ASCII'case letterCase superset)

deriving newtype instance Semigroup superset =>
    Semigroup (ASCII'case letterCase superset)

deriving newtype instance Monoid superset =>
    Monoid (ASCII'case letterCase superset)

deriving stock instance (Data superset, Typeable letterCase) =>
    Data (ASCII'case letterCase superset)

deriving stock instance Generic (ASCII'case letterCase superset)

instance Show superset => Show (ASCII'case letterCase superset) where
    showsPrec d x = showParen (d > app_prec) $
        showString "asciiCaseUnsafe " . showsPrec (succ app_prec) (lift x)
      where app_prec = 10

    showList x = showString "asciiCaseUnsafe " . showList (List.map lift x)

instance Superset.ToCaselessChar char => Superset.ToCaselessChar (ASCII'case letterCase char) where
    isAsciiCaselessChar _ = Bool.True
    toCaselessCharUnsafe = Superset.toCaselessCharUnsafe . lift

instance Superset.CharSuperset char => Superset.ToChar (ASCII'case letterCase char) where
    isAsciiChar _ = Bool.True
    toCharUnsafe = Superset.toCharUnsafe . lift

instance Superset.ToCaselessString string => Superset.ToCaselessString (ASCII'case letterCase string) where
    isAsciiCaselessString _ = Bool.True
    toCaselessCharListUnsafe = Superset.toCaselessCharListUnsafe . lift
    toCaselessCharListSub = Superset.toCaselessCharListSub . lift

instance Superset.ToString string => Superset.ToString (ASCII'case letterCase string) where
    isAsciiString _ = Bool.True
    toCharListUnsafe = Superset.toCharListUnsafe . lift
    toCharListSub = Superset.toCharListUnsafe . lift

{-| Change the type of an ASCII superset value that is known to be valid ASCII where
    letters are restricted to the 'Case' designated by the @letterCase@ type variable

This is "unsafe" because this assertion is unchecked, so this function is capable
of producing an invalid 'ASCII'case' value. -}
asciiCaseUnsafe :: superset -> ASCII'case letterCase superset
asciiCaseUnsafe = ASCII'case_Unsafe

---

{- $aliases

The 'ASCII'upper' and 'ASCII'lower' type aliases exist primarily so that you can
use 'ASCII'case' without the DataKinds language extension. -}
type ASCII'upper superset = ASCII'case 'UpperCase superset

type ASCII'lower superset = ASCII'case 'LowerCase superset

---

class KnownCase (letterCase :: Case) where theCase :: Case
instance KnownCase 'UpperCase where theCase = UpperCase
instance KnownCase 'LowerCase where theCase = LowerCase

---

{-| Return 'Just' an 'ASCII'case' character if the input is an ASCII character
    in the proper case, or 'Nothing' otherwise -}
validateChar :: forall letterCase superset. KnownCase letterCase => CharSuperset superset =>
    superset {- ^ Character which may or may not be in the ASCII character set;
                  if a letter, may be in any case -}
    -> Maybe (ASCII'case letterCase superset)
validateChar x = do
    c <- Superset.toCharMaybe x
    guard (Bool.not (Case.isCase (Case.opposite (theCase @letterCase)) c))
    Just (asciiCaseUnsafe x)

{-| Return an 'ASCII'case' character if the input is an ASCII character in the
    proper case, or 'ASCII.Substitute' otherwise -}
substituteChar :: forall letterCase superset. KnownCase letterCase => CharSuperset superset =>
    superset
    -> ASCII'case letterCase superset
substituteChar x = case validateChar x of
    Nothing -> asciiCaseUnsafe (Superset.fromChar ASCII.Substitute)
    Just c -> c

{-| Lift a 'CaselessChar' into a superset type, wrapped in the 'ASCII'case'
    refinement to save the evidence that it is ASCII in a particular case -}
fromCaselessChar :: forall letterCase superset. KnownCase letterCase => CharSuperset superset =>
    CaselessChar -- ^ Character which, if it is a letter, does not have a specified case
    -> ASCII'case letterCase superset
fromCaselessChar = asciiCaseUnsafe . Superset.fromChar . Caseless.toCase (theCase @letterCase)

{-| Given a character from some type that is known to represent an ASCII
    character in a particular case, obtain the caseless ASCII character
    it represents -}
toCaselessChar :: CharSuperset superset =>
    ASCII'case letterCase superset {- ^ Character that is known to be ASCII, and
                                        in the particular case if it is a letter -}
    -> CaselessChar
toCaselessChar = Caseless.disregardCase . Superset.toCharUnsafe . lift

{-| Given a character from a larger set that is known to represent an ASCII
    character, manipulate it as if it were an ASCII character -}
asCaselessChar :: forall letterCase superset. KnownCase letterCase => CharSuperset superset =>
    (CaselessChar -> CaselessChar) -- ^ Case-insensitive function over ASCII characters
    -> ASCII'case letterCase superset {- ^ Character that is known to be ASCII, and
                                           in the particular case if it is a letter -}
    -> ASCII'case letterCase superset
asCaselessChar f = asciiCaseUnsafe . Superset.asCharUnsafe g . lift
  where
    g = Caseless.toCase (theCase @letterCase) . f . Caseless.assumeCaseUnsafe (theCase @letterCase)

{-| Given an ASCII superset character that is known to be valid ASCII,
    refine it further by converting it to a particular letter case -}
refineCharToCase :: forall letterCase char. KnownCase letterCase => CharSuperset char =>
    ASCII char -> ASCII'case letterCase char
refineCharToCase = asciiCaseUnsafe . Superset.toCaseChar (theCase @letterCase) . Refinement.lift

---

{-| Return 'Just' an 'ASCII'case' string if the input consists entirely of ASCII
    characters in the proper case, or 'Nothing' otherwise -}
validateString :: forall letterCase superset. KnownCase letterCase => StringSuperset superset =>
    superset -- ^ String which may or may not be valid ASCII, where letters may be in any case
    -> Maybe (ASCII'case letterCase superset)
validateString x = do
    s <- Superset.toCharListMaybe x
    guard (Bool.not (any (Case.isCase (Case.opposite (theCase @letterCase))) s))
    Just (asciiCaseUnsafe x)

{-| Lift a list of 'CaselessChar' into a superset string type, wrapped in the
    'ASCII'case' refinement to save the evidence that all of the characters in
    the string are ASCII in a particular case -}
fromCaselessCharList :: forall letterCase superset. KnownCase letterCase => StringSuperset superset =>
    [CaselessChar] -- ^ Case-insensitive ASCII string represented as a list of caseless characters
    -> ASCII'case letterCase superset
fromCaselessCharList = asciiCaseUnsafe . Superset.fromCharList . List.map (Caseless.toCase (theCase @letterCase))

{-| Given a string from some type that is known to represent only ASCII characters
    in a particular case, obtain the caseless characters it represents -}
toCaselessCharList :: forall letterCase superset. KnownCase letterCase => StringSuperset superset =>
    ASCII'case letterCase superset -- ^ String that is known to be valid ASCII in a particular case
    -> [CaselessChar]
toCaselessCharList = List.map (Caseless.assumeCaseUnsafe (theCase @letterCase)) . Superset.toCharListUnsafe . lift

{-| Forces a string from a larger character set into cased ASCII by using the
    'ASCII.Substitute' character in place of any unacceptable characters -}
substituteString :: forall letterCase superset. KnownCase letterCase => StringSuperset superset =>
    superset -- ^ String which may or may not be valid ASCII, where letters may be in any case
    -> ASCII'case letterCase superset
substituteString = asciiCaseUnsafe . Superset.fromCharList . List.map f . Superset.toCharListSub
  where
    f x = if Case.isCase (Case.opposite (theCase @letterCase)) x
          then ASCII.Substitute
          else x

{-| Given a string from a larger set that is known to consist entirely of ASCII
    characters in a particular case, map over the characters in the string as if
    they were caseless ASCII characters -}
mapChars :: forall letterCase superset. KnownCase letterCase => StringSuperset superset =>
    (CaselessChar -> CaselessChar) -- ^ Case-insensitive function over ASCII characters
    -> ASCII'case letterCase superset -- ^ String that is known to be valid ASCII in a particular case
    -> ASCII'case letterCase superset
mapChars f = asciiCaseUnsafe . Superset.mapCharsUnsafe g . lift
  where
    g = Caseless.toCase (theCase @letterCase) . f . Caseless.assumeCaseUnsafe (theCase @letterCase)

{-| Given an ASCII superset string that is known to be valid ASCII,
refine it further by converting it to a particular letter case -}
refineStringToCase :: forall letterCase char. KnownCase letterCase => StringSuperset char =>
    ASCII char -> ASCII'case letterCase char
refineStringToCase = asciiCaseUnsafe . Superset.toCaseString (theCase @letterCase) . Refinement.lift
