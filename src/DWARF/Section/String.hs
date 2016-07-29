{-# LANGUAGE BangPatterns #-}
module DWARF.Section.String where

import           Data.Serialize(getBytes,lookAhead,runGet)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Word

import DWARF.Basics

stringFrom :: Sections -> Word64 -> Either String ByteString
stringFrom secs offset = runGet string (BS.drop (fromIntegral offset) bs)
  where bs = sectionBytes ".debug_str" secs



