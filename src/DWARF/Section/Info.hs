{-# LANGUAGE RecordWildCards #-}
module DWARF.Section.Info where

import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import           Data.Word
import           Data.Serialize

import DWARF.Basics
import DWARF.DIE
import DWARF.Section.Abbrev

data CU = CU
  { cuHeader     :: Header
  , cuSections   :: Sections
  , cuAbbr       :: Map Integer Abbreviation
  , cuOurOffset  :: !Word64
    -- ^ Our offset in 'debug_info', used to resolve local refs
  }

instance HasDIE CU where
  dieSections       = cuSections
  dieFormat         = format . cuHeader
  dieAddressSize    = address_size . cuHeader
  dieAbbr m i       = Map.lookup i (cuAbbr m)
  dieLocaslBytes CU { .. } n
    = BS.drop (fromIntegral (cuOurOffset + n))
        (sectionBytes ".debug_info" cuSections)

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



-- | Get the compilation unit ath the given offset.
getCU :: Sections -> Word64 -> Either String (CU,DIE)
getCU secs offset = flip runGet start $
  do hdr <- header (sectionEndian secs)
     abr <- case abbrev secs (abbr_offset hdr) of
              Left err -> fail err
              Right a  -> return a

     let cu = CU { cuHeader     = hdr
                 , cuSections   = secs
                 , cuAbbr       = abr
                 , cuOurOffset  = offset
                 }
     mb <- getDIE cu
     case mb of
       Nothing -> fail "(No debug info entry)"
       Just die -> return (cu, die)
  where
  debug_info = sectionBytes ".debug_info" secs
  start      = BS.drop (fromIntegral offset) debug_info






