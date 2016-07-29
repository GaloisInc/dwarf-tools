module DWARF.Utils where

import Numeric(showHex)

prettyHex :: (Integral a, Show a) => a -> String
prettyHex x = "0x" ++ replicate padding '0' ++ bs
  where
  bs = showHex x ""
  n  = length bs
  padding
    | n < 2     = 2 - n
    | n < 4     = 4 - n
    | n < 8     = 8 - n
    | otherwise = 0


