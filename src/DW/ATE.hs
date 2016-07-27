{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module DW.ATE where

import Data.Word
import DW.Basics(prettyHex)

newtype ATE = ATE Word64
              deriving (Eq,Ord)

pattern Address            = ATE 0x01
pattern Boolean            = ATE 0x02
pattern Complex_float      = ATE 0x03
pattern Float              = ATE 0x04
pattern Signed             = ATE 0x05
pattern Signed_char        = ATE 0x06
pattern Unsigned           = ATE 0x07
pattern Unsigned_char      = ATE 0x08
pattern Imaginary_float    = ATE 0x09
pattern Packed_decimal     = ATE 0x0a
pattern Numeric_string     = ATE 0x0b
pattern Edited             = ATE 0x0c
pattern Signed_fixed       = ATE 0x0d
pattern Unsigned_fixed     = ATE 0x0e
pattern Decimal_float      = ATE 0x0f
pattern UTF                = ATE 0x10
pattern Lo_user            = ATE 0x80
pattern Hi_user            = ATE 0xff

instance Show ATE where
  show x =
    case x of
      Address           -> "Address"
      Boolean           -> "Boolean"
      Complex_float     -> "Complex_float"
      Float             -> "Float"
      Signed            -> "Signed"
      Signed_char       -> "Signed_char"
      Unsigned          -> "Unsigned"
      Unsigned_char     -> "Unsigned_char"
      Imaginary_float   -> "Imaginary_float"
      Packed_decimal    -> "Packed_decimal"
      Numeric_string    -> "Numeric_string"
      Edited            -> "Edited"
      Signed_fixed      -> "Signed_fixed"
      Unsigned_fixed    -> "Unsigned_fixed"
      Decimal_float     -> "Decimal_float"
      UTF               -> "UTF"
      Lo_user           -> "Lo_user"
      Hi_user           -> "Hi_user"
      ATE w             -> prettyHex w

