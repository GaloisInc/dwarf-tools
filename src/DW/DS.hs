{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module DW.DS where

import Data.Word
import DW.Basics(prettyHex)

newtype DS = DS Word64
              deriving (Eq,Ord)

pattern Unsigned              = DS 0x01
pattern Leading_overpunch     = DS 0x02
pattern Trailing_overpunch    = DS 0x03
pattern Leading_separate      = DS 0x04
pattern Trailing_separate     = DS 0x05

instance Show DS where
  show x =
    case x of
      Unsigned             -> "Unsigned"
      Leading_overpunch    -> "Leading_overpunch"
      Trailing_overpunch   -> "Trailing_overpunch"
      Leading_separate     -> "Leading_separate"
      Trailing_separate    -> "Trailing_separate"
      DS w                 -> prettyHex w
