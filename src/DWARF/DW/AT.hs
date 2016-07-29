{-# LANGUAGE PatternSynonyms, CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif
module DWARF.DW.AT where

import Data.Word(Word64)
import DWARF.Utils(prettyHex)

newtype DW_AT = DW_AT Word64
                    deriving (Eq,Ord)

pattern DW_AT_NULL                    = DW_AT 0x00
pattern DW_AT_sibling                 = DW_AT 0x01
pattern DW_AT_location                = DW_AT 0x02
pattern DW_AT_name                    = DW_AT 0x03
pattern DW_AT_ordering                = DW_AT 0x09
pattern DW_AT_byte_size               = DW_AT 0x0b
pattern DW_AT_bit_offset              = DW_AT 0x0c
pattern DW_AT_bit_size                = DW_AT 0x0d
pattern DW_AT_stmt_list               = DW_AT 0x10
pattern DW_AT_low_pc                  = DW_AT 0x11
pattern DW_AT_high_pc                 = DW_AT 0x12
pattern DW_AT_language                = DW_AT 0x13
pattern DW_AT_discr                   = DW_AT 0x15
pattern DW_AT_discr_value             = DW_AT 0x16
pattern DW_AT_visibility              = DW_AT 0x17
pattern DW_AT_import                  = DW_AT 0x18
pattern DW_AT_string_length           = DW_AT 0x19
pattern DW_AT_common_reference        = DW_AT 0x1a
pattern DW_AT_comp_dir                = DW_AT 0x1b
pattern DW_AT_const_value             = DW_AT 0x1c
pattern DW_AT_containing_type         = DW_AT 0x1d
pattern DW_AT_default_value           = DW_AT 0x1e
pattern DW_AT_inline                  = DW_AT 0x20
pattern DW_AT_is_optional             = DW_AT 0x21
pattern DW_AT_lower_bound             = DW_AT 0x22
pattern DW_AT_producer                = DW_AT 0x25
pattern DW_AT_prototyped              = DW_AT 0x27
pattern DW_AT_return_addr             = DW_AT 0x2a
pattern DW_AT_start_scope             = DW_AT 0x2c
pattern DW_AT_bit_stride              = DW_AT 0x2e
pattern DW_AT_upper_bound             = DW_AT 0x2f
pattern DW_AT_abstract_origin         = DW_AT 0x31
pattern DW_AT_accessibility           = DW_AT 0x32
pattern DW_AT_address_class           = DW_AT 0x33
pattern DW_AT_artificial              = DW_AT 0x34
pattern DW_AT_base_types              = DW_AT 0x35
pattern DW_AT_calling_convention      = DW_AT 0x36
pattern DW_AT_count                   = DW_AT 0x37
pattern DW_AT_data_member_location    = DW_AT 0x38
pattern DW_AT_decl_column             = DW_AT 0x39
pattern DW_AT_decl_file               = DW_AT 0x3a
pattern DW_AT_decl_line               = DW_AT 0x3b
pattern DW_AT_declaration             = DW_AT 0x3c
pattern DW_AT_discr_list              = DW_AT 0x3d
pattern DW_AT_encoding                = DW_AT 0x3e
pattern DW_AT_external                = DW_AT 0x3f
pattern DW_AT_frame_base              = DW_AT 0x40
pattern DW_AT_friend                  = DW_AT 0x41
pattern DW_AT_identifier_case         = DW_AT 0x42
pattern DW_AT_macro_info              = DW_AT 0x43
pattern DW_AT_namelist_item           = DW_AT 0x44
pattern DW_AT_priority                = DW_AT 0x45
pattern DW_AT_segment                 = DW_AT 0x46
pattern DW_AT_specification           = DW_AT 0x47
pattern DW_AT_static_link             = DW_AT 0x48
pattern DW_AT_type                    = DW_AT 0x49
pattern DW_AT_use_location            = DW_AT 0x4a
pattern DW_AT_variable_parameter      = DW_AT 0x4b
pattern DW_AT_virtuality              = DW_AT 0x4c
pattern DW_AT_vtable_elem_location    = DW_AT 0x4d
pattern DW_AT_allocated               = DW_AT 0x4e
pattern DW_AT_associated              = DW_AT 0x4f
pattern DW_AT_data_location           = DW_AT 0x50
pattern DW_AT_byte_stride             = DW_AT 0x51
pattern DW_AT_entry_pc                = DW_AT 0x52
pattern DW_AT_use_UTF8                = DW_AT 0x53
pattern DW_AT_extension               = DW_AT 0x54
pattern DW_AT_ranges                  = DW_AT 0x55
pattern DW_AT_trampoline              = DW_AT 0x56
pattern DW_AT_call_column             = DW_AT 0x57
pattern DW_AT_call_file               = DW_AT 0x58
pattern DW_AT_call_line               = DW_AT 0x59
pattern DW_AT_description             = DW_AT 0x5a
pattern DW_AT_binary_scale            = DW_AT 0x5b
pattern DW_AT_decimal_scale           = DW_AT 0x5c
pattern DW_AT_small                   = DW_AT 0x5d
pattern DW_AT_decimal_sign            = DW_AT 0x5e
pattern DW_AT_digit_count             = DW_AT 0x5f
pattern DW_AT_picture_string          = DW_AT 0x60
pattern DW_AT_mutable                 = DW_AT 0x61
pattern DW_AT_threads_scaled          = DW_AT 0x62
pattern DW_AT_explicit                = DW_AT 0x63
pattern DW_AT_object_pointer          = DW_AT 0x64
pattern DW_AT_endianity               = DW_AT 0x65
pattern DW_AT_elemental               = DW_AT 0x66
pattern DW_AT_pure                    = DW_AT 0x67
pattern DW_AT_recursive               = DW_AT 0x68
pattern DW_AT_signature               = DW_AT 0x69
pattern DW_AT_main_subprogram         = DW_AT 0x6a
pattern DW_AT_data_bit_offset         = DW_AT 0x6b
pattern DW_AT_const_expr              = DW_AT 0x6c
pattern DW_AT_enum_class              = DW_AT 0x6d
pattern DW_AT_linkage_name            = DW_AT 0x6e
pattern DW_AT_lo_user                 = DW_AT 0x2000
pattern DW_AT_hi_user                 = DW_AT 0x3ff

instance Show DW_AT where
  show a =
    case a of
      DW_AT_NULL                  -> "DW_AT_NULL"
      DW_AT_sibling               -> "DW_AT_sibling"
      DW_AT_location              -> "DW_AT_location"
      DW_AT_name                  -> "DW_AT_name"
      DW_AT_ordering              -> "DW_AT_ordering"
      DW_AT_byte_size             -> "DW_AT_byte_size"
      DW_AT_bit_offset            -> "DW_AT_bit_offset"
      DW_AT_bit_size              -> "DW_AT_bit_size"
      DW_AT_stmt_list             -> "DW_AT_stmt_list"
      DW_AT_low_pc                -> "DW_AT_low_pc"
      DW_AT_high_pc               -> "DW_AT_high_pc"
      DW_AT_language              -> "DW_AT_language"
      DW_AT_discr                 -> "DW_AT_discr"
      DW_AT_discr_value           -> "DW_AT_discr_value"
      DW_AT_visibility            -> "DW_AT_visibility"
      DW_AT_import                -> "DW_AT_import"
      DW_AT_string_length         -> "DW_AT_string_length"
      DW_AT_common_reference      -> "DW_AT_common_reference"
      DW_AT_comp_dir              -> "DW_AT_comp_dir"
      DW_AT_const_value           -> "DW_AT_const_value"
      DW_AT_containing_type       -> "DW_AT_containing_type"
      DW_AT_default_value         -> "DW_AT_default_value"
      DW_AT_inline                -> "DW_AT_inline"
      DW_AT_is_optional           -> "DW_AT_is_optional"
      DW_AT_lower_bound           -> "DW_AT_lower_bound"
      DW_AT_producer              -> "DW_AT_producer"
      DW_AT_prototyped            -> "DW_AT_prototyped"
      DW_AT_return_addr           -> "DW_AT_return_addr"
      DW_AT_start_scope           -> "DW_AT_start_scope"
      DW_AT_bit_stride            -> "DW_AT_bit_stride"
      DW_AT_upper_bound           -> "DW_AT_upper_bound"
      DW_AT_abstract_origin       -> "DW_AT_abstract_origin"
      DW_AT_accessibility         -> "DW_AT_accessibility"
      DW_AT_address_class         -> "DW_AT_address_class"
      DW_AT_artificial            -> "DW_AT_artificial"
      DW_AT_base_types            -> "DW_AT_base_types"
      DW_AT_calling_convention    -> "DW_AT_calling_convention"
      DW_AT_count                 -> "DW_AT_count"
      DW_AT_data_member_location  -> "DW_AT_data_member_location"
      DW_AT_decl_column           -> "DW_AT_decl_column"
      DW_AT_decl_file             -> "DW_AT_decl_file"
      DW_AT_decl_line             -> "DW_AT_decl_line"
      DW_AT_declaration           -> "DW_AT_declaration"
      DW_AT_discr_list            -> "DW_AT_discr_list"
      DW_AT_encoding              -> "DW_AT_encoding"
      DW_AT_external              -> "DW_AT_external"
      DW_AT_frame_base            -> "DW_AT_frame_base"
      DW_AT_friend                -> "DW_AT_friend"
      DW_AT_identifier_case       -> "DW_AT_identifier_case"
      DW_AT_macro_info            -> "DW_AT_macro_info"
      DW_AT_namelist_item         -> "DW_AT_namelist_item"
      DW_AT_priority              -> "DW_AT_priority"
      DW_AT_segment               -> "DW_AT_segment"
      DW_AT_specification         -> "DW_AT_specification"
      DW_AT_static_link           -> "DW_AT_static_link"
      DW_AT_type                  -> "DW_AT_type"
      DW_AT_use_location          -> "DW_AT_use_location"
      DW_AT_variable_parameter    -> "DW_AT_variable_parameter"
      DW_AT_virtuality            -> "DW_AT_virtuality"
      DW_AT_vtable_elem_location  -> "DW_AT_vtable_elem_location"
      DW_AT_allocated             -> "DW_AT_allocated"
      DW_AT_associated            -> "DW_AT_associated"
      DW_AT_data_location         -> "DW_AT_data_location"
      DW_AT_byte_stride           -> "DW_AT_byte_stride"
      DW_AT_entry_pc              -> "DW_AT_entry_pc"
      DW_AT_use_UTF8              -> "DW_AT_use_UTF8"
      DW_AT_extension             -> "DW_AT_extension"
      DW_AT_ranges                -> "DW_AT_ranges"
      DW_AT_trampoline            -> "DW_AT_trampoline"
      DW_AT_call_column           -> "DW_AT_call_column"
      DW_AT_call_file             -> "DW_AT_call_file"
      DW_AT_call_line             -> "DW_AT_call_line"
      DW_AT_description           -> "DW_AT_description"
      DW_AT_binary_scale          -> "DW_AT_binary_scale"
      DW_AT_decimal_scale         -> "DW_AT_decimal_scale"
      DW_AT_small                 -> "DW_AT_small"
      DW_AT_decimal_sign          -> "DW_AT_decimal_sign"
      DW_AT_digit_count           -> "DW_AT_digit_count"
      DW_AT_picture_string        -> "DW_AT_picture_string"
      DW_AT_mutable               -> "DW_AT_mutable"
      DW_AT_threads_scaled        -> "DW_AT_threads_scaled"
      DW_AT_explicit              -> "DW_AT_explicit"
      DW_AT_object_pointer        -> "DW_AT_object_pointer"
      DW_AT_endianity             -> "DW_AT_endianity"
      DW_AT_elemental             -> "DW_AT_elemental"
      DW_AT_pure                  -> "DW_AT_pure"
      DW_AT_recursive             -> "DW_AT_recursive"
      DW_AT_signature             -> "DW_AT_signature"
      DW_AT_main_subprogram       -> "DW_AT_main_subprogram"
      DW_AT_data_bit_offset       -> "DW_AT_data_bit_offset"
      DW_AT_const_expr            -> "DW_AT_const_expr"
      DW_AT_enum_class            -> "DW_AT_enum_class"
      DW_AT_linkage_name          -> "DW_AT_linkage_name"
      DW_AT_lo_user               -> "DW_AT_lo_user"
      DW_AT_hi_user               -> "DW_AT_hi_user"
      DW_AT x                     -> prettyHex x
