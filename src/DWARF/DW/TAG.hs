{-# LANGUAGE PatternSynonyms, CPP #-}
#if __GLASGOW_HASKELL >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif
module DWARF.DW.TAG where

import Data.Word(Word64)
import DWARF.Utils(prettyHex)

newtype DW_TAG = DW_TAG Word64
              deriving (Eq,Ord)

pattern DW_TAG_NULL                     = DW_TAG 0x00
pattern DW_TAG_array_type               = DW_TAG 0x01
pattern DW_TAG_class_type               = DW_TAG 0x02
pattern DW_TAG_entry_point              = DW_TAG 0x03
pattern DW_TAG_enumeration_type         = DW_TAG 0x04
pattern DW_TAG_formal_parameter         = DW_TAG 0x05
pattern DW_TAG_imported_declaration     = DW_TAG 0x08
pattern DW_TAG_label                    = DW_TAG 0x0a
pattern DW_TAG_lexical_block            = DW_TAG 0x0b
pattern DW_TAG_member                   = DW_TAG 0x0d
pattern DW_TAG_pointer_type             = DW_TAG 0x0f
pattern DW_TAG_reference_type           = DW_TAG 0x10
pattern DW_TAG_compile_unit             = DW_TAG 0x11
pattern DW_TAG_string_type              = DW_TAG 0x12
pattern DW_TAG_structure_type           = DW_TAG 0x13
pattern DW_TAG_subroutine_type          = DW_TAG 0x15
pattern DW_TAG_typedef                  = DW_TAG 0x16
pattern DW_TAG_union_type               = DW_TAG 0x17
pattern DW_TAG_unspecified_parameters   = DW_TAG 0x18
pattern DW_TAG_variant                  = DW_TAG 0x19
pattern DW_TAG_common_block             = DW_TAG 0x1a
pattern DW_TAG_common_inclusion         = DW_TAG 0x1b
pattern DW_TAG_inheritance              = DW_TAG 0x1c
pattern DW_TAG_inlined_subroutine       = DW_TAG 0x1d
pattern DW_TAG_module                   = DW_TAG 0x1e
pattern DW_TAG_ptr_to_member_type       = DW_TAG 0x1f
pattern DW_TAG_set_type                 = DW_TAG 0x20
pattern DW_TAG_subrange_type            = DW_TAG 0x21
pattern DW_TAG_with_stmt                = DW_TAG 0x22
pattern DW_TAG_access_declaration       = DW_TAG 0x23
pattern DW_TAG_base_type                = DW_TAG 0x24
pattern DW_TAG_catch_block              = DW_TAG 0x25
pattern DW_TAG_const_type               = DW_TAG 0x26
pattern DW_TAG_constant                 = DW_TAG 0x27
pattern DW_TAG_enumerator               = DW_TAG 0x28
pattern DW_TAG_file_type                = DW_TAG 0x29
pattern DW_TAG_friend                   = DW_TAG 0x2a
pattern DW_TAG_namelist                 = DW_TAG 0x2b
pattern DW_TAG_namelist_item            = DW_TAG 0x2c
pattern DW_TAG_packed_type              = DW_TAG 0x2d
pattern DW_TAG_subprogram               = DW_TAG 0x2e
pattern DW_TAG_template_type_parameter  = DW_TAG 0x2f
pattern DW_TAG_template_value_parameter = DW_TAG 0x30
pattern DW_TAG_thrown_type              = DW_TAG 0x31
pattern DW_TAG_try_block                = DW_TAG 0x32
pattern DW_TAG_variant_part             = DW_TAG 0x33
pattern DW_TAG_variable                 = DW_TAG 0x34
pattern DW_TAG_volatile_type            = DW_TAG 0x35
pattern DW_TAG_dwarf_procedure          = DW_TAG 0x36
pattern DW_TAG_restrict_type            = DW_TAG 0x37
pattern DW_TAG_interface_type           = DW_TAG 0x38
pattern DW_TAG_namespace                = DW_TAG 0x39
pattern DW_TAG_imported_module          = DW_TAG 0x3a
pattern DW_TAG_unspecified_type         = DW_TAG 0x3b
pattern DW_TAG_partial_unit             = DW_TAG 0x3c
pattern DW_TAG_imported_unit            = DW_TAG 0x3d
pattern DW_TAG_condition                = DW_TAG 0x3f
pattern DW_TAG_shared_type              = DW_TAG 0x40
pattern DW_TAG_type_unit                = DW_TAG 0x41
pattern DW_TAG_rvalue_reference_type    = DW_TAG 0x42
pattern DW_TAG_template_alias           = DW_TAG 0x43
pattern DW_TAG_lo_user                  = DW_TAG 0x4080
pattern DW_TAG_hi_user                  = DW_TAG 0xffff



instance Show DW_TAG where
  show t =
    case t of
      DW_TAG_NULL                       -> "DW_TAG_NULL"
      DW_TAG_array_type                 -> "DW_TAG_array_type"
      DW_TAG_class_type                 -> "DW_TAG_class_type"
      DW_TAG_entry_point                -> "DW_TAG_entry_point"
      DW_TAG_enumeration_type           -> "DW_TAG_enumeration_type"
      DW_TAG_formal_parameter           -> "DW_TAG_formal_parameter"
      DW_TAG_imported_declaration       -> "DW_TAG_imported_declaration"
      DW_TAG_label                      -> "DW_TAG_label"
      DW_TAG_lexical_block              -> "DW_TAG_lexical_block"
      DW_TAG_member                     -> "DW_TAG_member"
      DW_TAG_pointer_type               -> "DW_TAG_pointer_type"
      DW_TAG_reference_type             -> "DW_TAG_reference_type"
      DW_TAG_compile_unit               -> "DW_TAG_compile_unit"
      DW_TAG_string_type                -> "DW_TAG_string_type"
      DW_TAG_structure_type             -> "DW_TAG_structure_type"
      DW_TAG_subroutine_type            -> "DW_TAG_subroutine_type"
      DW_TAG_typedef                    -> "DW_TAG_typedef"
      DW_TAG_union_type                 -> "DW_TAG_union_type"
      DW_TAG_unspecified_parameters     -> "DW_TAG_unspecified_parameters"
      DW_TAG_variant                    -> "DW_TAG_variant"
      DW_TAG_common_block               -> "DW_TAG_common_block"
      DW_TAG_common_inclusion           -> "DW_TAG_common_inclusion"
      DW_TAG_inheritance                -> "DW_TAG_inheritance"
      DW_TAG_inlined_subroutine         -> "DW_TAG_inlined_subroutine"
      DW_TAG_module                     -> "DW_TAG_module"
      DW_TAG_ptr_to_member_type         -> "DW_TAG_ptr_to_member_type"
      DW_TAG_set_type                   -> "DW_TAG_set_type"
      DW_TAG_subrange_type              -> "DW_TAG_subrange_type"
      DW_TAG_with_stmt                  -> "DW_TAG_with_stmt"
      DW_TAG_access_declaration         -> "DW_TAG_access_declaration"
      DW_TAG_base_type                  -> "DW_TAG_base_type"
      DW_TAG_catch_block                -> "DW_TAG_catch_block"
      DW_TAG_const_type                 -> "DW_TAG_const_type"
      DW_TAG_constant                   -> "DW_TAG_constant"
      DW_TAG_enumerator                 -> "DW_TAG_enumerator"
      DW_TAG_file_type                  -> "DW_TAG_file_type"
      DW_TAG_friend                     -> "DW_TAG_friend"
      DW_TAG_namelist                   -> "DW_TAG_namelist"
      DW_TAG_namelist_item              -> "DW_TAG_namelist_item"
      DW_TAG_packed_type                -> "DW_TAG_packed_type"
      DW_TAG_subprogram                 -> "DW_TAG_subprogram"
      DW_TAG_template_type_parameter    -> "DW_TAG_template_type_parameter"
      DW_TAG_template_value_parameter   -> "DW_TAG_template_value_parameter"
      DW_TAG_thrown_type                -> "DW_TAG_thrown_type"
      DW_TAG_try_block                  -> "DW_TAG_try_block"
      DW_TAG_variant_part               -> "DW_TAG_variant_part"
      DW_TAG_variable                   -> "DW_TAG_variable"
      DW_TAG_volatile_type              -> "DW_TAG_volatile_type"
      DW_TAG_dwarf_procedure            -> "DW_TAG_dwarf_procedure"
      DW_TAG_restrict_type              -> "DW_TAG_restrict_type"
      DW_TAG_interface_type             -> "DW_TAG_interface_type"
      DW_TAG_namespace                  -> "DW_TAG_namespace"
      DW_TAG_imported_module            -> "DW_TAG_imported_module"
      DW_TAG_unspecified_type           -> "DW_TAG_unspecified_type"
      DW_TAG_partial_unit               -> "DW_TAG_partial_unit"
      DW_TAG_imported_unit              -> "DW_TAG_imported_unit"
      DW_TAG_condition                  -> "DW_TAG_condition"
      DW_TAG_shared_type                -> "DW_TAG_shared_type"
      DW_TAG_type_unit                  -> "DW_TAG_type_unit"
      DW_TAG_rvalue_reference_type      -> "DW_TAG_rvalue_reference_type"
      DW_TAG_template_alias             -> "DW_TAG_template_alias"
      DW_TAG_lo_user                    -> "DW_TAG_lo_user"
      DW_TAG_hi_user                    -> "DW_TAG_hi_user"
      DW_TAG x                          -> prettyHex x




