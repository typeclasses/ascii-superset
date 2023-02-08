module ASCII.SupersetConversion
  (
    {- * Class -} StringSupersetConversion (..),
    {- * Utilities -} convertRefinedString,
  )
  where

import ASCII.Superset

import {-# source #-} ASCII.Refinement.Internal (convertRefinedString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

class (StringSuperset a, StringSuperset b) => StringSupersetConversion a b where
    convertStringUnsafe :: a -> b

instance StringSupersetConversion T.Text BS.ByteString where
    convertStringUnsafe = T.encodeUtf8

instance StringSupersetConversion BS.ByteString T.Text where
    convertStringUnsafe = T.decodeASCII

instance StringSupersetConversion LT.Text LBS.ByteString where
    convertStringUnsafe = LT.encodeUtf8

instance StringSupersetConversion LBS.ByteString LT.Text where
    convertStringUnsafe = LT.decodeASCII

instance StringSupersetConversion T.Text LT.Text where
    convertStringUnsafe = LT.fromStrict

instance StringSupersetConversion LT.Text T.Text where
    convertStringUnsafe = LT.toStrict

instance StringSupersetConversion BS.ByteString LBS.ByteString where
    convertStringUnsafe = LBS.fromStrict

instance StringSupersetConversion LBS.ByteString BS.ByteString where
    convertStringUnsafe = LBS.toStrict
