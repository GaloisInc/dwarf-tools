{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module DW.LANG where

import Data.Word
import DW.Basics(prettyHex)

newtype LANG = LANG Word64
              deriving (Eq,Ord)

pattern C89               = LANG 0x0001
pattern C                 = LANG 0x0002
pattern Ada83             = LANG 0x0003
pattern C_plus_plus       = LANG 0x0004
pattern Cobol74           = LANG 0x0005
pattern Cobol85           = LANG 0x0006
pattern Fortran77         = LANG 0x0007
pattern Fortran90         = LANG 0x0008
pattern Pascal83          = LANG 0x0009
pattern Modula2           = LANG 0x000a
pattern Java              = LANG 0x000b
pattern C99               = LANG 0x000c
pattern Ada95             = LANG 0x000d
pattern Fortran95         = LANG 0x000e
pattern PLI               = LANG 0x000f
pattern ObjC              = LANG 0x0010
pattern ObjC_plus_plus    = LANG 0x0011
pattern UPC               = LANG 0x0012
pattern D                 = LANG 0x0013
pattern Python            = LANG 0x0014
pattern Lo_user           = LANG 0x8000
pattern Hi_user           = LANG 0xffff

instance Show LANG where
  show x =
    case x of
      C89              -> "C89"
      C                -> "C"
      Ada83            -> "Ada83"
      C_plus_plus      -> "C_plus_plus"
      Cobol74          -> "Cobol74"
      Cobol85          -> "Cobol85"
      Fortran77        -> "Fortran77"
      Fortran90        -> "Fortran90"
      Pascal83         -> "Pascal83"
      Modula2          -> "Modula2"
      Java             -> "Java"
      C99              -> "C99"
      Ada95            -> "Ada95"
      Fortran95        -> "Fortran95"
      PLI              -> "PLI"
      ObjC             -> "ObjC"
      ObjC_plus_plus   -> "ObjC_plus_plus"
      UPC              -> "UPC"
      D                -> "D"
      Python           -> "Python"
      Lo_user          -> "Lo_user"
      Hi_user          -> "Hi_user"
      LANG w           -> prettyHex w


