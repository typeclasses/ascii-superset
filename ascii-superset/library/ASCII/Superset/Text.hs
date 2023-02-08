module ASCII.Superset.Text where

import ASCII.CaseRefinement (ASCII'case)
import ASCII.Refinement (ASCII)
import Data.Function (id, (.))

import qualified ASCII.CaseRefinement as CaseRefinement
import qualified ASCII.Refinement as Refinement
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.Char as Unicode
import qualified Data.Text as Strict (Text)
import qualified Data.Text as Text.Strict
import qualified Data.Text.Encoding as Text.Strict
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy

class ToText a where
    toStrictText :: a -> Strict.Text
    toStrictText = Text.Lazy.toStrict . toLazyText

    toLazyText :: a -> Lazy.Text
    toLazyText = Text.Lazy.fromStrict . toStrictText

    toUnicodeCharList :: a -> [Unicode.Char]
    toUnicodeCharList = Text.Lazy.unpack . toLazyText

    {-# minimal toStrictText | toLazyText #-}

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
