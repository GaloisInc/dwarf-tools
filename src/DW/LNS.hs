{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module DW.LNS where

import Data.Word

newtype Standard = Standard Word8

pattern Copy                  = Standard 0x01
pattern Advance_pc            = Standard 0x02
pattern Advance_line          = Standard 0x03
pattern Set_file              = Standard 0x04
pattern Set_column            = Standard 0x05
pattern Negate_stmt           = Standard 0x06
pattern Set_basic_block       = Standard 0x07
pattern Const_add_pc          = Standard 0x08
pattern Fixed_advance_pc      = Standard 0x09
pattern Set_prologue_end      = Standard 0x0a
pattern Set_epilogue_begin    = Standard 0x0b
pattern Set_isa               = Standard 0x0c



newtype Extended = Extended Word8

pattern End_sequence          = Extended 0x01
pattern Set_address           = Extended 0x02
pattern Define_file           = Extended 0x03
pattern Set_discriminator     = Extended 0x04
pattern Lo_user               = Extended 0x80
pattern Hi_user               = Extended 0xff

