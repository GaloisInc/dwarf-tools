{-# LANGUAGE PatternSynonyms, CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif
module DWARF.DW.LANG where

import Data.Word(Word64)
import DWARF.Utils(prettyHex)

newtype DW_LANG = DW_LANG Word64
              deriving (Eq,Ord)

pattern DW_LANG_C89               = DW_LANG 0x0001
pattern DW_LANG_C                 = DW_LANG 0x0002
pattern DW_LANG_Ada83             = DW_LANG 0x0003
pattern DW_LANG_C_plus_plus       = DW_LANG 0x0004
pattern DW_LANG_Cobol74           = DW_LANG 0x0005
pattern DW_LANG_Cobol85           = DW_LANG 0x0006
pattern DW_LANG_Fortran77         = DW_LANG 0x0007
pattern DW_LANG_Fortran90         = DW_LANG 0x0008
pattern DW_LANG_Pascal83          = DW_LANG 0x0009
pattern DW_LANG_Modula2           = DW_LANG 0x000a
pattern DW_LANG_Java              = DW_LANG 0x000b
pattern DW_LANG_C99               = DW_LANG 0x000c
pattern DW_LANG_Ada95             = DW_LANG 0x000d
pattern DW_LANG_Fortran95         = DW_LANG 0x000e
pattern DW_LANG_PLI               = DW_LANG 0x000f
pattern DW_LANG_ObjC              = DW_LANG 0x0010
pattern DW_LANG_ObjC_plus_plus    = DW_LANG 0x0011
pattern DW_LANG_UPC               = DW_LANG 0x0012
pattern DW_LANG_D                 = DW_LANG 0x0013
pattern DW_LANG_Python            = DW_LANG 0x0014
pattern DW_LANG_Lo_user           = DW_LANG 0x8000
pattern DW_LANG_Hi_user           = DW_LANG 0xffff

instance Show DW_LANG where
  show x =
    case x of
      DW_LANG_C89              -> "DW_LANG_C89"
      DW_LANG_C                -> "DW_LANG_C"
      DW_LANG_Ada83            -> "DW_LANG_Ada83"
      DW_LANG_C_plus_plus      -> "DW_LANG_C_plus_plus"
      DW_LANG_Cobol74          -> "DW_LANG_Cobol74"
      DW_LANG_Cobol85          -> "DW_LANG_Cobol85"
      DW_LANG_Fortran77        -> "DW_LANG_Fortran77"
      DW_LANG_Fortran90        -> "DW_LANG_Fortran90"
      DW_LANG_Pascal83         -> "DW_LANG_Pascal83"
      DW_LANG_Modula2          -> "DW_LANG_Modula2"
      DW_LANG_Java             -> "DW_LANG_Java"
      DW_LANG_C99              -> "DW_LANG_C99"
      DW_LANG_Ada95            -> "DW_LANG_Ada95"
      DW_LANG_Fortran95        -> "DW_LANG_Fortran95"
      DW_LANG_PLI              -> "DW_LANG_PLI"
      DW_LANG_ObjC             -> "DW_LANG_ObjC"
      DW_LANG_ObjC_plus_plus   -> "DW_LANG_ObjC_plus_plus"
      DW_LANG_UPC              -> "DW_LANG_UPC"
      DW_LANG_D                -> "DW_LANG_D"
      DW_LANG_Python           -> "DW_LANG_Python"
      DW_LANG_Lo_user          -> "DW_LANG_Lo_user"
      DW_LANG_Hi_user          -> "DW_LANG_Hi_user"
      DW_LANG w                -> prettyHex w


