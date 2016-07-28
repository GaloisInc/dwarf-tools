{-# LANGUAGE BangPatterns #-}
module DW.Section.String where

import           Data.Serialize(Get,getBytes,lookAhead,runGet)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Word

import DW.Basics
import DW.Sections

string :: Get ByteString
string = do bs <- getBytes =<< lookAhead (findZero 0)
            _  <- word8 -- skip the 0 terminator
            return bs
  where
  findZero !n = do b <- word8
                   if b == 0 then return n else findZero (n + 1)

stringFrom :: Sections -> Word64 -> Either String ByteString
stringFrom secs offset = runGet string (BS.drop (fromIntegral offset) bs)
  where bs = sectionBytes ".debug_str" secs



