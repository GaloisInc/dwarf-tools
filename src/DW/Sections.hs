module DW.Sections where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import DW.Basics
import DW.Section.Abbrev

data Sections = Sections
  { secEndian   :: Endian
  , secAbbrev   :: Map Integer Abbreviation
  , secBytes    :: Map String ByteString
  }

sections :: Endian -> Map String ByteString -> Either String Sections
sections endian mp =
  do secAbbrev <- abbrev (getBytes ".debug_abbrev")
     return Sections { secEndian  = endian
                     , secBytes   = mp
                     , secAbbrev  = secAbbrev
                     }
  where
  getBytes x = Map.findWithDefault BS.empty x mp

sectionBytes :: String -> Sections -> ByteString
sectionBytes x s = Map.findWithDefault BS.empty x (secBytes s)

sectionAbbr :: Integer -> Sections -> Maybe Abbreviation
sectionAbbr n sec = Map.lookup n (secAbbrev sec)


