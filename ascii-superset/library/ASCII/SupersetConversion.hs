module ASCII.SupersetConversion
  ( -- * Class
    StringSupersetConversion (..),

    -- * Utilities
    convertRefinedString,
  )
where

import {-# SOURCE #-} ASCII.Refinement.Internal (convertRefinedString)
import ASCII.Superset
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.Char qualified as Unicode
import Data.Kind (Type)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT

class
  (StringSuperset a, StringSuperset b) =>
  StringSupersetConversion (a :: Type) (b :: Type)
  where
  convertStringUnsafe :: a -> b

instance StringSupersetConversion T.Text BS.ByteString where
  convertStringUnsafe = T.encodeUtf8

instance StringSupersetConversion BS.ByteString T.Text where
  convertStringUnsafe = T.decodeUtf8

instance StringSupersetConversion LT.Text LBS.ByteString where
  convertStringUnsafe = LT.encodeUtf8

instance StringSupersetConversion LBS.ByteString LT.Text where
  convertStringUnsafe = LT.decodeUtf8

instance StringSupersetConversion T.Text LT.Text where
  convertStringUnsafe = LT.fromStrict

instance StringSupersetConversion LT.Text T.Text where
  convertStringUnsafe = LT.toStrict

instance StringSupersetConversion BS.ByteString LBS.ByteString where
  convertStringUnsafe = LBS.fromStrict

instance StringSupersetConversion LBS.ByteString BS.ByteString where
  convertStringUnsafe = LBS.toStrict

---

instance StringSupersetConversion T.Text [Unicode.Char] where
  convertStringUnsafe = T.unpack

instance StringSupersetConversion [Unicode.Char] T.Text where
  convertStringUnsafe = T.pack

instance StringSupersetConversion LT.Text [Unicode.Char] where
  convertStringUnsafe = LT.unpack

instance StringSupersetConversion [Unicode.Char] LT.Text where
  convertStringUnsafe = LT.pack

instance StringSupersetConversion BS.ByteString [Unicode.Char] where
  convertStringUnsafe = BS.Char8.unpack

instance StringSupersetConversion [Unicode.Char] BS.ByteString where
  convertStringUnsafe = BS.Char8.pack

instance StringSupersetConversion LBS.ByteString [Unicode.Char] where
  convertStringUnsafe = LBS.Char8.unpack

instance StringSupersetConversion [Unicode.Char] LBS.ByteString where
  convertStringUnsafe = LBS.Char8.pack
