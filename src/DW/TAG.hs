{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module DW.TAG where

import Data.Word

import DW.Basics(prettyHex)

newtype TAG = TAG Word64
              deriving (Eq,Ord)

pattern NULL                     = TAG 0x00
pattern Array_type               = TAG 0x01
pattern Class_type               = TAG 0x02
pattern Entry_point              = TAG 0x03
pattern Enumeration_type         = TAG 0x04
pattern Formal_parameter         = TAG 0x05
pattern Imported_declaration     = TAG 0x08
pattern Label                    = TAG 0x0a
pattern Lexical_block            = TAG 0x0b
pattern Member                   = TAG 0x0d
pattern Pointer_type             = TAG 0x0f
pattern Reference_type           = TAG 0x10
pattern Compile_unit             = TAG 0x11
pattern String_type              = TAG 0x12
pattern Structure_type           = TAG 0x13
pattern Subroutine_type          = TAG 0x15
pattern Typedef                  = TAG 0x16
pattern Union_type               = TAG 0x17
pattern Unspecified_parameters   = TAG 0x18
pattern Variant                  = TAG 0x19
pattern Common_block             = TAG 0x1a
pattern Common_inclusion         = TAG 0x1b
pattern Inheritance              = TAG 0x1c
pattern Inlined_subroutine       = TAG 0x1d
pattern Module                   = TAG 0x1e
pattern Ptr_to_member_type       = TAG 0x1f
pattern Set_type                 = TAG 0x20
pattern Subrange_type            = TAG 0x21
pattern With_stmt                = TAG 0x22
pattern Access_declaration       = TAG 0x23
pattern Base_type                = TAG 0x24
pattern Catch_block              = TAG 0x25
pattern Const_type               = TAG 0x26
pattern Constant                 = TAG 0x27
pattern Enumerator               = TAG 0x28
pattern File_type                = TAG 0x29
pattern Friend                   = TAG 0x2a
pattern Namelist                 = TAG 0x2b
pattern Namelist_item            = TAG 0x2c
pattern Packed_type              = TAG 0x2d
pattern Subprogram               = TAG 0x2e
pattern Template_type_parameter  = TAG 0x2f
pattern Template_value_parameter = TAG 0x30
pattern Thrown_type              = TAG 0x31
pattern Try_block                = TAG 0x32
pattern Variant_part             = TAG 0x33
pattern Variable                 = TAG 0x34
pattern Volatile_type            = TAG 0x35
pattern Dwarf_procedure          = TAG 0x36
pattern Restrict_type            = TAG 0x37
pattern Interface_type           = TAG 0x38
pattern Namespace                = TAG 0x39
pattern Imported_module          = TAG 0x3a
pattern Unspecified_type         = TAG 0x3b
pattern Partial_unit             = TAG 0x3c
pattern Imported_unit            = TAG 0x3d
pattern Condition                = TAG 0x3f
pattern Shared_type              = TAG 0x40
pattern Type_unit                = TAG 0x41
pattern Rvalue_reference_type    = TAG 0x42
pattern Template_alias           = TAG 0x43
pattern Lo_user                  = TAG 0x4080
pattern Hi_user                  = TAG 0xffff



instance Show TAG where
  show t =
    case t of
      NULL                       -> "NULL"
      Array_type                 -> "Array_type"
      Class_type                 -> "Class_type"
      Entry_point                -> "Entry_point"
      Enumeration_type           -> "Enumeration_type"
      Formal_parameter           -> "Formal_parameter"
      Imported_declaration       -> "Imported_declaration"
      Label                      -> "Label"
      Lexical_block              -> "Lexical_block"
      Member                     -> "Member"
      Pointer_type               -> "Pointer_type"
      Reference_type             -> "Reference_type"
      Compile_unit               -> "Compile_unit"
      String_type                -> "String_type"
      Structure_type             -> "Structure_type"
      Subroutine_type            -> "Subroutine_type"
      Typedef                    -> "Typedef"
      Union_type                 -> "Union_type"
      Unspecified_parameters     -> "Unspecified_parameters"
      Variant                    -> "Variant"
      Common_block               -> "Common_block"
      Common_inclusion           -> "Common_inclusion"
      Inheritance                -> "Inheritance"
      Inlined_subroutine         -> "Inlined_subroutine"
      Module                     -> "Module"
      Ptr_to_member_type         -> "Ptr_to_member_type"
      Set_type                   -> "Set_type"
      Subrange_type              -> "Subrange_type"
      With_stmt                  -> "With_stmt"
      Access_declaration         -> "Access_declaration"
      Base_type                  -> "Base_type"
      Catch_block                -> "Catch_block"
      Const_type                 -> "Const_type"
      Constant                   -> "Constant"
      Enumerator                 -> "Enumerator"
      File_type                  -> "File_type"
      Friend                     -> "Friend"
      Namelist                   -> "Namelist"
      Namelist_item              -> "Namelist_item"
      Packed_type                -> "Packed_type"
      Subprogram                 -> "Subprogram"
      Template_type_parameter    -> "Template_type_parameter"
      Template_value_parameter   -> "Template_value_parameter"
      Thrown_type                -> "Thrown_type"
      Try_block                  -> "Try_block"
      Variant_part               -> "Variant_part"
      Variable                   -> "Variable"
      Volatile_type              -> "Volatile_type"
      Dwarf_procedure            -> "Dwarf_procedure"
      Restrict_type              -> "Restrict_type"
      Interface_type             -> "Interface_type"
      Namespace                  -> "Namespace"
      Imported_module            -> "Imported_module"
      Unspecified_type           -> "Unspecified_type"
      Partial_unit               -> "Partial_unit"
      Imported_unit              -> "Imported_unit"
      Condition                  -> "Condition"
      Shared_type                -> "Shared_type"
      Type_unit                  -> "Type_unit"
      Rvalue_reference_type      -> "Rvalue_reference_type"
      Template_alias             -> "Template_alias"
      Lo_user                    -> "Lo_user"
      Hi_user                    -> "Hi_user"
      TAG x                      -> prettyHex x




