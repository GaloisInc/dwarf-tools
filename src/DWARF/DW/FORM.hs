{-# LANGUAGE PatternSynonyms, CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif
module DWARF.DW.FORM where

import Data.Word(Word64)
import DWARF.Utils(prettyHex)

newtype DW_FORM = DW_FORM Word64
               deriving (Eq,Ord)

pattern DW_FORM_NULL         = DW_FORM 0x00
pattern DW_FORM_addr         = DW_FORM 0x01
pattern DW_FORM_block2       = DW_FORM 0x03
pattern DW_FORM_block4       = DW_FORM 0x04
pattern DW_FORM_data2        = DW_FORM 0x05
pattern DW_FORM_data4        = DW_FORM 0x06
pattern DW_FORM_data8        = DW_FORM 0x07
pattern DW_FORM_string       = DW_FORM 0x08
pattern DW_FORM_block        = DW_FORM 0x09
pattern DW_FORM_block1       = DW_FORM 0x0a
pattern DW_FORM_data1        = DW_FORM 0x0b
pattern DW_FORM_flag         = DW_FORM 0x0c
pattern DW_FORM_sdata        = DW_FORM 0x0d
pattern DW_FORM_strp         = DW_FORM 0x0e
pattern DW_FORM_udata        = DW_FORM 0x0f
pattern DW_FORM_ref_addr     = DW_FORM 0x10
pattern DW_FORM_ref1         = DW_FORM 0x11
pattern DW_FORM_ref2         = DW_FORM 0x12
pattern DW_FORM_ref4         = DW_FORM 0x13
pattern DW_FORM_ref8         = DW_FORM 0x14
pattern DW_FORM_ref_udata    = DW_FORM 0x15
pattern DW_FORM_indirect     = DW_FORM 0x16
pattern DW_FORM_sec_offset   = DW_FORM 0x17
pattern DW_FORM_exprloc      = DW_FORM 0x18
pattern DW_FORM_flag_present = DW_FORM 0x19
pattern DW_FORM_ref_sig8     = DW_FORM 0x20

instance Show DW_FORM where
  show f =
    case f of
      DW_FORM_NULL           -> "DW_FORM_NULL"
      DW_FORM_addr           -> "DW_FORM_addr"
      DW_FORM_block2         -> "DW_FORM_block2"
      DW_FORM_block4         -> "DW_FORM_block4"
      DW_FORM_data2          -> "DW_FORM_data2"
      DW_FORM_data4          -> "DW_FORM_data4"
      DW_FORM_data8          -> "DW_FORM_data8"
      DW_FORM_string         -> "DW_FORM_string"
      DW_FORM_block          -> "DW_FORM_block"
      DW_FORM_block1         -> "DW_FORM_block1"
      DW_FORM_data1          -> "DW_FORM_data1"
      DW_FORM_flag           -> "DW_FORM_flag"
      DW_FORM_sdata          -> "DW_FORM_sdata"
      DW_FORM_strp           -> "DW_FORM_strp"
      DW_FORM_udata          -> "DW_FORM_udata"
      DW_FORM_ref_addr       -> "DW_FORM_ref_addr"
      DW_FORM_ref1           -> "DW_FORM_ref1"
      DW_FORM_ref2           -> "DW_FORM_ref2"
      DW_FORM_ref4           -> "DW_FORM_ref4"
      DW_FORM_ref8           -> "DW_FORM_ref8"
      DW_FORM_ref_udata      -> "DW_FORM_ref_udata"
      DW_FORM_indirect       -> "DW_FORM_indirect"
      DW_FORM_sec_offset     -> "DW_FORM_sec_offset"
      DW_FORM_exprloc        -> "DW_FORM_exprloc"
      DW_FORM_flag_present   -> "DW_FORM_flag_present"
      DW_FORM_ref_sig8       -> "DW_FORM_ref_sig8"
      DW_FORM x         -> prettyHex x


