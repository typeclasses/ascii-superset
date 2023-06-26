module ASCII.Superset.Text where

import ASCII.CaseRefinement (ASCII'case)
import ASCII.CaseRefinement qualified as CaseRefinement
import ASCII.Refinement (ASCII)
import ASCII.Refinement qualified as Refinement
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Char qualified as Unicode
import Data.Function (id, (.))
import Data.Kind (Type)
import Data.Text qualified as Strict (Text)
import Data.Text qualified as Text.Strict
import Data.Text.Encoding qualified as Text.Strict
import Data.Text.Lazy qualified as Lazy (Text)
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy

class ToText (a :: Type) where
  toStrictText :: a -> Strict.Text
  toStrictText = Text.Lazy.toStrict . toLazyText

  toLazyText :: a -> Lazy.Text
  toLazyText = Text.Lazy.fromStrict . toStrictText

  toUnicodeCharList :: a -> [Unicode.Char]
  toUnicodeCharList = Text.Lazy.unpack . toLazyText

  {-# MINIMAL toStrictText | toLazyText #-}

instance ToText Strict.Text where
  toStrictText = id

instance ToText Lazy.Text where
  toLazyText = id

instance ToText [Unicode.Char] where
  toUnicodeCharList = id
  toStrictText = Text.Strict.pack
  toLazyText = Text.Lazy.pack

instance ToText (ASCII Strict.Text) where
  toStrictText = Refinement.lift

instance ToText (ASCII Lazy.Text) where
  toLazyText = Refinement.lift

instance ToText (ASCII Strict.ByteString) where
  toStrictText = Text.Strict.decodeUtf8 . Refinement.lift

instance ToText (ASCII Lazy.ByteString) where
  toLazyText = Text.Lazy.decodeUtf8 . Refinement.lift

instance ToText (ASCII [Unicode.Char]) where
  toUnicodeCharList = Refinement.lift
  toStrictText = Text.Strict.pack . Refinement.lift
  toLazyText = Text.Lazy.pack . Refinement.lift

instance ToText (ASCII'case letterCase Strict.Text) where
  toStrictText = Refinement.lift . CaseRefinement.forgetCase

instance ToText (ASCII'case letterCase Lazy.Text) where
  toLazyText = Refinement.lift . CaseRefinement.forgetCase

instance ToText (ASCII'case letterCase Strict.ByteString) where
  toStrictText = Text.Strict.decodeUtf8 . Refinement.lift . CaseRefinement.forgetCase

instance ToText (ASCII'case letterCase Lazy.ByteString) where
  toLazyText = Text.Lazy.decodeUtf8 . Refinement.lift . CaseRefinement.forgetCase

instance ToText (ASCII'case letterCase [Unicode.Char]) where
  toUnicodeCharList = Refinement.lift . CaseRefinement.forgetCase
  toStrictText = Text.Strict.pack . Refinement.lift . CaseRefinement.forgetCase
  toLazyText = Text.Lazy.pack . Refinement.lift . CaseRefinement.forgetCase
