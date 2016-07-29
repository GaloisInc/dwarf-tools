{-# LANGUAGE PatternSynonyms, CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif
module DWARF.DW.ATE where

import Data.Word(Word64)
import DWARF.Utils(prettyHex)

newtype DW_ATE = DW_ATE Word64
              deriving (Eq,Ord)

pattern DW_ATE_address            = DW_ATE 0x01
pattern DW_ATE_boolean            = DW_ATE 0x02
pattern DW_ATE_complex_float      = DW_ATE 0x03
pattern DW_ATE_float              = DW_ATE 0x04
pattern DW_ATE_signed             = DW_ATE 0x05
pattern DW_ATE_signed_char        = DW_ATE 0x06
pattern DW_ATE_unsigned           = DW_ATE 0x07
pattern DW_ATE_unsigned_char      = DW_ATE 0x08
pattern DW_ATE_imaginary_float    = DW_ATE 0x09
pattern DW_ATE_packed_decimal     = DW_ATE 0x0a
pattern DW_ATE_numeric_string     = DW_ATE 0x0b
pattern DW_ATE_edited             = DW_ATE 0x0c
pattern DW_ATE_signed_fixed       = DW_ATE 0x0d
pattern DW_ATE_unsigned_fixed     = DW_ATE 0x0e
pattern DW_ATE_decimal_float      = DW_ATE 0x0f
pattern DW_ATE_uTF                = DW_ATE 0x10
pattern DW_ATE_lo_user            = DW_ATE 0x80
pattern DW_ATE_hi_user            = DW_ATE 0xff

instance Show DW_ATE where
  show x =
    case x of
      DW_ATE_address           -> "DW_ATE_address"
      DW_ATE_boolean           -> "DW_ATE_boolean"
      DW_ATE_complex_float     -> "DW_ATE_complex_float"
      DW_ATE_float             -> "DW_ATE_float"
      DW_ATE_signed            -> "DW_ATE_signed"
      DW_ATE_signed_char       -> "DW_ATE_signed_char"
      DW_ATE_unsigned          -> "DW_ATE_unsigned"
      DW_ATE_unsigned_char     -> "DW_ATE_unsigned_char"
      DW_ATE_imaginary_float   -> "DW_ATE_imaginary_float"
      DW_ATE_packed_decimal    -> "DW_ATE_packed_decimal"
      DW_ATE_numeric_string    -> "DW_ATE_numeric_string"
      DW_ATE_edited            -> "DW_ATE_edited"
      DW_ATE_signed_fixed      -> "DW_ATE_signed_fixed"
      DW_ATE_unsigned_fixed    -> "DW_ATE_unsigned_fixed"
      DW_ATE_decimal_float     -> "DW_ATE_decimal_float"
      DW_ATE_uTF               -> "DW_ATE_uTF"
      DW_ATE_lo_user           -> "DW_ATE_lo_user"
      DW_ATE_hi_user           -> "DW_ATE_hi_user"
      DW_ATE w                 -> prettyHex w

