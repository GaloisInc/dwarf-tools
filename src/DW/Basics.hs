{-# LANGUAGE BangPatterns #-}
module DW.Basics where

import Data.Serialize.Get(Get)
import qualified Data.Serialize as S
import Data.Word(Word8,Word16,Word32,Word64)
import Data.Int(Int8)
import Data.Bits(shiftL,clearBit,testBit,(.|.))
import Numeric(showHex)

data Endian       = LittleEndian | BigEndian
                    deriving Show

data DwarfFormat  = Dwarf32 | Dwarf64
                    deriving Show

int8 :: Get Int8
int8 = S.getInt8

word8 :: Get Word8
word8 = S.getWord8

word16 :: Endian -> Get Word16
word16 i =
  case i of
    LittleEndian -> S.getWord16le
    BigEndian    -> S.getWord16be

word32 :: Endian -> Get Word32
word32 i =
  case i of
    LittleEndian -> S.getWord32le
    BigEndian    -> S.getWord32be

word64 :: Endian -> Get Word64
word64 i =
  case i of
    LittleEndian -> S.getWord64le
    BigEndian    -> S.getWord64be


leb128 :: Get (Int,Integer)
leb128 = go 0 0
  where
  combine sh by res = shiftL (fromIntegral (clearBit by 7)) sh .|. res

  go !res !sh =
    do byte <- word8
       let newRes = combine sh byte res
       if testBit byte 7
         then go newRes (sh + 7)
         else do let signBit = sh + 6
                 signBit `seq` newRes `seq` return (signBit,newRes)


uleb_from_leb :: (Int,Integer) -> Integer
uleb_from_leb = snd

sleb_from_leb :: (Int,Integer) -> Integer
sleb_from_leb (sign,w)
  | testBit w sign  = fromIntegral w .|. negate (shiftL 1 sign)
  | otherwise       = w


uleb128 :: Get Integer
uleb128 = fmap uleb_from_leb leb128

sleb128 :: Get Integer
sleb128 = fmap sleb_from_leb leb128

initialLength :: Endian -> Get (DwarfFormat, Word64)
initialLength chunkEndian =
  do mbSize <- word32 chunkEndian
     case mbSize of
       0xffffffff ->
        do size <- word64 chunkEndian
           return (Dwarf64, size)

       _ | mbSize < 0xfffffff0 ->
           return (Dwarf32, fromIntegral mbSize)

         | otherwise -> fail ("Unknown section size: " ++ prettyHex mbSize)


word :: Endian -> DwarfFormat-> Get Word64
word endian format =
  case format of
    Dwarf32 -> fromIntegral <$> word32 endian
    Dwarf64 -> word64 endian


unumber :: Endian -> Word8 -> Get Integer
unumber endian sz = go 0 0
  where
  go !num !ix = if ix < sz
                  then do b <- word8
                          go (combine ix b num) (ix + 1)
                  else return num

  combine ix b num =
    case endian of
      BigEndian    -> shiftL num 8 .|. fromIntegral b
      LittleEndian -> shiftL (fromIntegral b) (8 * fromIntegral ix) .|. num


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


