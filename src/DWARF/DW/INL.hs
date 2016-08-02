{-# LANGUAGE PatternSynonyms, CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif
module DWARF.DW.INL where

import Data.Word(Word64)
import DWARF.Utils(prettyHex)

newtype DW_INL = DW_INL Word64

pattern DW_INL_not_inlined             = DW_INL 0x00
pattern DW_INL_inlined                 = DW_INL 0x01
pattern DW_INL_declared_not_inlined    = DW_INL 0x02
pattern DW_INL_declared_inlined        = DW_INL 0x03

instance Show DW_INL where
  show x =
    case x of
      DW_INL_not_inlined            -> "DW_INL_not_inlined"
      DW_INL_inlined                -> "DW_INL_inlined"
      DW_INL_declared_not_inlined   -> "DW_INL_declared_not_inlined"
      DW_INL_declared_inlined       -> "DW_INL_declared_inlined"
      DW_INL x                      -> prettyHex x

