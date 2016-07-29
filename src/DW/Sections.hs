module DW.Sections where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import DW.Basics

data Sections = Sections
  { secEndian   :: Endian
  , secBytes    :: Map String ByteString
  }

sections :: Endian -> Map String ByteString -> Sections
sections endian mp = Sections { secEndian  = endian
                              , secBytes   = mp
                              }

sectionBytes :: String -> Sections -> ByteString
sectionBytes x s = Map.findWithDefault BS.empty x (secBytes s)



