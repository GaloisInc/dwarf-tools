{-# LANGUAGE RecordWildCards #-}
module DW.Section.Info where

import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Word
import           Data.Serialize

import DW.Basics
import DW.DIE
import DW.Sections
import DW.Section.Abbrev

data Meta = Meta
  { metaHeader     :: Header
  , metaSections   :: Sections
  , metaAbbr       :: Map Integer Abbreviation
  }

instance DieMeta Meta where
  dieSections     = metaSections
  dieFormat       = format . metaHeader
  dieAddressSize  = address_size . metaHeader
  dieAbbr m i     = Map.lookup i (metaAbbr m)

data Header = Header
  { format        :: !DwarfFormat
  , size          :: !Word64
  , version       :: !Word16
  , abbr_offset   :: !Word64
  , address_size  :: !Word8
  } deriving Show

header :: Endian -> Get Header
header endian =
  do (format,size) <- initialLength endian
     version       <- word16 endian
     abbr_offset   <- word endian format
     address_size  <- word8
     return Header { .. }

getDIE :: Sections -> Get DIE
getDIE secs =
  do hdr <- header (secEndian secs)
     abr <- case abbrev secs (abbr_offset hdr) of
              Left err -> fail err
              Right a  -> return a

     let meta = Meta { metaHeader   = hdr
                     , metaSections = secs
                     , metaAbbr     = abr
                     }
     mb <- debugInfoEntry meta
     case mb of
       Nothing -> fail "(No debug info entry)"
       Just die -> return die

dieFrom :: Sections -> Word64 -> Either String DIE
dieFrom secs offset = runGet (getDIE secs)
                    $ BS.drop (fromIntegral offset)
                    $ sectionBytes ".debug_info" secs


