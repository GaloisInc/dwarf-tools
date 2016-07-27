{-# LANGUAGE RecordWildCards #-}
module DW.Section.Line where

import Data.Word
import Data.Int
import Data.Serialize
import Data.ByteString(ByteString)

import DW.Basics

data Header = Header
  { format                             :: !DwarfFormat
  , size                               :: !Word64
  , version                            :: !Word16
  , header_length                      :: !Word64
  , minimum_instruction_length         :: !Word8
  , maximum_operations_per_instruction :: !Word8
  , default_is_stmt                    :: !Word8
  , line_base                          :: !Int8
  , line_range                         :: !Word8
  , opcode_base                        :: !Word8
  , standard_opcode_lengths            :: !ByteString -- Array of Word8
  , include_directories                :: [ByteString] -- ?
  , file_names                         :: [ByteString]
  }


header :: Endian -> Get Header
header endian =
  do (format,size)                      <- initialLength endian
     version                            <- word16 endian
     header_length                      <- word endian format
     minimum_instruction_length         <- word8
     maximum_operations_per_instruction <- word8
     default_is_stmt                    <- word8
     line_base                          <- int8
     line_range                         <- word8
     opcode_base                        <- word8
     standard_opcode_lengths <- getBytes (fromIntegral opcode_base - 1)
     include_directories <- return [] -- XXX
     file_names          <- return [] -- XXX
     return Header { .. }


