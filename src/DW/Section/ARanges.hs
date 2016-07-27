{-# LANGUAGE RecordWildCards #-}
module DW.Section.ARanges (Entry(..), aranges) where

import           Data.Serialize.Get(Get,runGet,skip)
import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Control.Applicative(many)

import DW.Basics
import DW.Sections


aranges :: Sections -> Either String [([Entry], Word64)]
aranges secs = runGet (entrySets (secEndian secs))
             $ sectionBytes ".debug_aranges" secs

entrySets :: Endian -> Get [ ([Entry], Word64) ]
entrySets endian = many (entrySet endian)

data Header = Header
  { arFormat          :: !DwarfFormat
  , arSize            :: !Word64
  , arVersion         :: !Word16
  , arDebugInfoOffset :: !Word64
  , arAddressSize     :: !Word8
  , arSegmentSize     :: !Word8
  } deriving Show


header :: Endian -> Get Header
header endian =
  do (arFormat,arSize) <- initialLength endian
     arVersion         <- word16 endian
     arDebugInfoOffset <- word endian arFormat
     arAddressSize     <- word8
     arSegmentSize     <- word8
     let entrySize = fromIntegral arSegmentSize + 2 * fromIntegral arAddressSize
         varSz     = case arFormat of
                       Dwarf32 -> 4 + 4
                       Dwarf64 -> 4 + 8 + 8
         hdrSz     = varSz + 2 + 1 + 1
     case mod hdrSz entrySize of
       0 -> return ()
       n -> skip (entrySize - n)
     return Header { .. }

data Entry = Entry { segment  :: Integer
                   , address  :: Integer
                   , size     :: Integer
                   } deriving Show

entry :: Endian -> Header -> Get (Maybe Entry)
entry endian Header { .. } =
  do segment <- unumber endian arSegmentSize
     address <- unumber endian arAddressSize
     size    <- unumber endian arAddressSize
     if segment == 0 && address == 0 && size == 0
       then return Nothing
       else return (Just Entry { .. })

entrySet :: Endian -> Get ([Entry], Word64)
entrySet endian =
  do h <- header endian
     go h []
  where
  go h acc = do mb <- entry endian h
                case mb of
                  Nothing -> return (reverse acc, arDebugInfoOffset h)
                  Just e  -> go h (e : acc)



