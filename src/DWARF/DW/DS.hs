{-# LANGUAGE PatternSynonyms, CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif
module DWARF.DW.DS where

import Data.Word(Word64)
import DWARF.Utils(prettyHex)

newtype DW_DS = DW_DS Word64
              deriving (Eq,Ord)

pattern DW_DW_unsigned              = DW_DS 0x01
pattern DW_DW_leading_overpunch     = DW_DS 0x02
pattern DW_DW_trailing_overpunch    = DW_DS 0x03
pattern DW_DW_leading_separate      = DW_DS 0x04
pattern DW_DW_trailing_separate     = DW_DS 0x05

instance Show DW_DS where
  show x =
    case x of
      DW_DW_unsigned            -> "unsigned"
      DW_DW_leading_overpunch   -> "leading_overpunch"
      DW_DW_trailing_overpunch  -> "trailing_overpunch"
      DW_DW_leading_separate    -> "leading_separate"
      DW_DW_trailing_separate   -> "trailing_separate"
      DW_DS w                   -> prettyHex w
