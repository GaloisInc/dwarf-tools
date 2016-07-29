{-# LANGUAGE PatternSynonyms, CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif
module DWARF.DW.LNS where

import Data.Word(Word8)

newtype DW_LNS_Standard = DW_LNS_Standard Word8

pattern DW_LNS_copy                  = DW_LNS_Standard 0x01
pattern DW_LNS_advance_pc            = DW_LNS_Standard 0x02
pattern DW_LNS_advance_line          = DW_LNS_Standard 0x03
pattern DW_LNS_set_file              = DW_LNS_Standard 0x04
pattern DW_LNS_set_column            = DW_LNS_Standard 0x05
pattern DW_LNS_negate_stmt           = DW_LNS_Standard 0x06
pattern DW_LNS_set_basic_block       = DW_LNS_Standard 0x07
pattern DW_LNS_const_add_pc          = DW_LNS_Standard 0x08
pattern DW_LNS_fixed_advance_pc      = DW_LNS_Standard 0x09
pattern DW_LNS_set_prologue_end      = DW_LNS_Standard 0x0a
pattern DW_LNS_set_epilogue_begin    = DW_LNS_Standard 0x0b
pattern DW_LNS_set_isa               = DW_LNS_Standard 0x0c



newtype DW_LNS_Extended = DW_LNS_Extended Word8

pattern DW_LNS_end_sequence          = DW_LNS_Extended 0x01
pattern DW_LNS_set_address           = DW_LNS_Extended 0x02
pattern DW_LNS_define_file           = DW_LNS_Extended 0x03
pattern DW_LNS_set_discriminator     = DW_LNS_Extended 0x04
pattern DW_LNS_lo_user               = DW_LNS_Extended 0x80
pattern DW_LNS_hi_user               = DW_LNS_Extended 0xff

