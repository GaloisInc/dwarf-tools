{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module DW.FORM where

import Data.Word
import DW.Basics(prettyHex)

newtype FORM = FORM Word64
               deriving (Eq,Ord)

pattern NULL         = FORM 0x00
pattern Addr         = FORM 0x01
pattern Block2       = FORM 0x03
pattern Block4       = FORM 0x04
pattern Data2        = FORM 0x05
pattern Data4        = FORM 0x06
pattern Data8        = FORM 0x07
pattern String       = FORM 0x08
pattern Block        = FORM 0x09
pattern Block1       = FORM 0x0a
pattern Data1        = FORM 0x0b
pattern Flag         = FORM 0x0c
pattern Sdata        = FORM 0x0d
pattern Strp         = FORM 0x0e
pattern Udata        = FORM 0x0f
pattern Ref_addr     = FORM 0x10
pattern Ref1         = FORM 0x11
pattern Ref2         = FORM 0x12
pattern Ref4         = FORM 0x13
pattern Ref8         = FORM 0x14
pattern Ref_udata    = FORM 0x15
pattern Indirect     = FORM 0x16
pattern Sec_offset   = FORM 0x17
pattern Exprloc      = FORM 0x18
pattern Flag_present = FORM 0x19
pattern Ref_sig8     = FORM 0x20

instance Show FORM where
  show f =
    case f of
      NULL           -> "NULL"
      Addr           -> "Addr"
      Block2         -> "Block2"
      Block4         -> "Block4"
      Data2          -> "Data2"
      Data4          -> "Data4"
      Data8          -> "Data8"
      String         -> "String"
      Block          -> "Block"
      Block1         -> "Block1"
      Data1          -> "Data1"
      Flag           -> "Flag"
      Sdata          -> "Sdata"
      Strp           -> "Strp"
      Udata          -> "Udata"
      Ref_addr       -> "Ref_addr"
      Ref1           -> "Ref1"
      Ref2           -> "Ref2"
      Ref4           -> "Ref4"
      Ref8           -> "Ref8"
      Ref_udata      -> "Ref_udata"
      Indirect       -> "Indirect"
      Sec_offset     -> "Sec_offset"
      Exprloc        -> "Exprloc"
      Flag_present   -> "Flag_present"
      Ref_sig8       -> "Ref_sig8"
      FORM x         -> prettyHex x


