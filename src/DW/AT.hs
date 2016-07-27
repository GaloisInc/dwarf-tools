{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module DW.AT where

import Data.Word
import DW.Basics(prettyHex)

newtype AT = AT Word64
                    deriving (Eq,Ord)

pattern NULL                    = AT 0x00
pattern Sibling                 = AT 0x01
pattern Location                = AT 0x02
pattern Name                    = AT 0x03
pattern Ordering                = AT 0x09
pattern Byte_size               = AT 0x0b
pattern Bit_offset              = AT 0x0c
pattern Bit_size                = AT 0x0d
pattern Stmt_list               = AT 0x10
pattern Low_pc                  = AT 0x11
pattern High_pc                 = AT 0x12
pattern Language                = AT 0x13
pattern Discr                   = AT 0x15
pattern Discr_value             = AT 0x16
pattern Visibility              = AT 0x17
pattern Import                  = AT 0x18
pattern String_length           = AT 0x19
pattern Common_reference        = AT 0x1a
pattern Comp_dir                = AT 0x1b
pattern Const_value             = AT 0x1c
pattern Containing_type         = AT 0x1d
pattern Default_value           = AT 0x1e
pattern Inline                  = AT 0x20
pattern Is_optional             = AT 0x21
pattern Lower_bound             = AT 0x22
pattern Producer                = AT 0x25
pattern Prototyped              = AT 0x27
pattern Return_addr             = AT 0x2a
pattern Start_scope             = AT 0x2c
pattern Bit_stride              = AT 0x2e
pattern Upper_bound             = AT 0x2f
pattern Abstract_origin         = AT 0x31
pattern Accessibility           = AT 0x32
pattern Address_class           = AT 0x33
pattern Artificial              = AT 0x34
pattern Base_types              = AT 0x35
pattern Calling_convention      = AT 0x36
pattern Count                   = AT 0x37
pattern Data_member_location    = AT 0x38
pattern Decl_column             = AT 0x39
pattern Decl_file               = AT 0x3a
pattern Decl_line               = AT 0x3b
pattern Declaration             = AT 0x3c
pattern Discr_list              = AT 0x3d
pattern Encoding                = AT 0x3e
pattern External                = AT 0x3f
pattern Frame_base              = AT 0x40
pattern Friend                  = AT 0x41
pattern Identifier_case         = AT 0x42
pattern Macro_info              = AT 0x43
pattern Namelist_item           = AT 0x44
pattern Priority                = AT 0x45
pattern Segment                 = AT 0x46
pattern Specification           = AT 0x47
pattern Static_link             = AT 0x48
pattern Type                    = AT 0x49
pattern Use_location            = AT 0x4a
pattern Variable_parameter      = AT 0x4b
pattern Virtuality              = AT 0x4c
pattern Vtable_elem_location    = AT 0x4d
pattern Allocated               = AT 0x4e
pattern Associated              = AT 0x4f
pattern Data_location           = AT 0x50
pattern Byte_stride             = AT 0x51
pattern Entry_pc                = AT 0x52
pattern Use_UTF8                = AT 0x53
pattern Extension               = AT 0x54
pattern Ranges                  = AT 0x55
pattern Trampoline              = AT 0x56
pattern Call_column             = AT 0x57
pattern Call_file               = AT 0x58
pattern Call_line               = AT 0x59
pattern Description             = AT 0x5a
pattern Binary_scale            = AT 0x5b
pattern Decimal_scale           = AT 0x5c
pattern Small                   = AT 0x5d
pattern Decimal_sign            = AT 0x5e
pattern Digit_count             = AT 0x5f
pattern Picture_string          = AT 0x60
pattern Mutable                 = AT 0x61
pattern Threads_scaled          = AT 0x62
pattern Explicit                = AT 0x63
pattern Object_pointer          = AT 0x64
pattern Endianity               = AT 0x65
pattern Elemental               = AT 0x66
pattern Pure                    = AT 0x67
pattern Recursive               = AT 0x68
pattern Signature               = AT 0x69
pattern Main_subprogram         = AT 0x6a
pattern Data_bit_offset         = AT 0x6b
pattern Const_expr              = AT 0x6c
pattern Enum_class              = AT 0x6d
pattern Linkage_name            = AT 0x6e
pattern Lo_user                 = AT 0x2000
pattern Hi_user                 = AT 0x3ff

instance Show AT where
  show a =
    case a of
      NULL                  -> "NULL"
      Sibling               -> "Sibling"
      Location              -> "Location"
      Name                  -> "Name"
      Ordering              -> "Ordering"
      Byte_size             -> "Byte_size"
      Bit_offset            -> "Bit_offset"
      Bit_size              -> "Bit_size"
      Stmt_list             -> "Stmt_list"
      Low_pc                -> "Low_pc"
      High_pc               -> "High_pc"
      Language              -> "Language"
      Discr                 -> "Discr"
      Discr_value           -> "Discr_value"
      Visibility            -> "Visibility"
      Import                -> "Import"
      String_length         -> "String_length"
      Common_reference      -> "Common_reference"
      Comp_dir              -> "Comp_dir"
      Const_value           -> "Const_value"
      Containing_type       -> "Containing_type"
      Default_value         -> "Default_value"
      Inline                -> "Inline"
      Is_optional           -> "Is_optional"
      Lower_bound           -> "Lower_bound"
      Producer              -> "Producer"
      Prototyped            -> "Prototyped"
      Return_addr           -> "Return_addr"
      Start_scope           -> "Start_scope"
      Bit_stride            -> "Bit_stride"
      Upper_bound           -> "Upper_bound"
      Abstract_origin       -> "Abstract_origin"
      Accessibility         -> "Accessibility"
      Address_class         -> "Address_class"
      Artificial            -> "Artificial"
      Base_types            -> "Base_types"
      Calling_convention    -> "Calling_convention"
      Count                 -> "Count"
      Data_member_location  -> "Data_member_location"
      Decl_column           -> "Decl_column"
      Decl_file             -> "Decl_file"
      Decl_line             -> "Decl_line"
      Declaration           -> "Declaration"
      Discr_list            -> "Discr_list"
      Encoding              -> "Encoding"
      External              -> "External"
      Frame_base            -> "Frame_base"
      Friend                -> "Friend"
      Identifier_case       -> "Identifier_case"
      Macro_info            -> "Macro_info"
      Namelist_item         -> "Namelist_item"
      Priority              -> "Priority"
      Segment               -> "Segment"
      Specification         -> "Specification"
      Static_link           -> "Static_link"
      Type                  -> "Type"
      Use_location          -> "Use_location"
      Variable_parameter    -> "Variable_parameter"
      Virtuality            -> "Virtuality"
      Vtable_elem_location  -> "Vtable_elem_location"
      Allocated             -> "Allocated"
      Associated            -> "Associated"
      Data_location         -> "Data_location"
      Byte_stride           -> "Byte_stride"
      Entry_pc              -> "Entry_pc"
      Use_UTF8              -> "Use_UTF8"
      Extension             -> "Extension"
      Ranges                -> "Ranges"
      Trampoline            -> "Trampoline"
      Call_column           -> "Call_column"
      Call_file             -> "Call_file"
      Call_line             -> "Call_line"
      Description           -> "Description"
      Binary_scale          -> "Binary_scale"
      Decimal_scale         -> "Decimal_scale"
      Small                 -> "Small"
      Decimal_sign          -> "Decimal_sign"
      Digit_count           -> "Digit_count"
      Picture_string        -> "Picture_string"
      Mutable               -> "Mutable"
      Threads_scaled        -> "Threads_scaled"
      Explicit              -> "Explicit"
      Object_pointer        -> "Object_pointer"
      Endianity             -> "Endianity"
      Elemental             -> "Elemental"
      Pure                  -> "Pure"
      Recursive             -> "Recursive"
      Signature             -> "Signature"
      Main_subprogram       -> "Main_subprogram"
      Data_bit_offset       -> "Data_bit_offset"
      Const_expr            -> "Const_expr"
      Enum_class            -> "Enum_class"
      Linkage_name          -> "Linkage_name"
      Lo_user               -> "Lo_user"
      Hi_user               -> "Hi_user"
      AT x                  -> prettyHex x
