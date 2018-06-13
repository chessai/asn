{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnboxedTuples #-}

module Asn
  ( Asn(..)
  , encode
  , decode
  ) where

import Data.Text (Text)
import Data.Primitive.Types (Prim)
import Data.Word (Word32)
import Data.Hashable (Hashable)
import Foreign.Storable (Storable)
import qualified Data.Aeson as AE
import qualified Data.Scientific as SCI
import qualified Data.Text as T
import qualified Data.Text.Read as TR

newtype Asn = Asn { getAsn :: Word32 }
  deriving (Show,Read,Eq,Ord,Prim,Hashable,Storable)

instance AE.ToJSON Asn where
  toJSON = AE.String . encode

instance AE.FromJSON Asn where
  parseJSON v = case v of
    AE.Object _ -> fail "expected ASN (String or Number), encountered Object"
    AE.Array _ -> fail "expected ASN (String or Number), encountered Array"
    AE.String t -> case decode t of
      Nothing -> fail "ASN string was invalid"
      Just w -> return w
    AE.Number n -> case SCI.toBoundedInteger n of
      Nothing -> fail "numeric ASN was invalid"
      Just w -> return (Asn w)
    AE.Bool _ -> fail "expected ASN (String or Number), encountered Bool"
    AE.Null -> fail "expected ASN (String or Number), encountered Null"

encode :: Asn -> Text
encode (Asn w) = T.pack (show w)

decode :: Text -> Maybe Asn
decode t
  | T.take 2 t == "AS" = decodeAsnDigits (T.drop 2 t)
  | otherwise = decodeAsnDigits t

decodeAsnDigits :: Text -> Maybe Asn
decodeAsnDigits t = case TR.decimal t of
  Left _ -> Nothing
  Right (w,extra) -> if T.null extra
    then Just (Asn w)
    else Nothing
