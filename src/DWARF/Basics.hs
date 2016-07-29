{-# LANGUAGE BangPatterns #-}
-- | Decoding basic values used in all DWARF components.
module DWARF.Basics
  ( -- * DWARF Sections
    Sections
  , sections
  , sectionBytes
  , sectionEndian
  , Endian(..)

    -- * Decoding of Common DWARF Values
  , int8
  , word8, word16, word32, word64, word, DwarfFormat(..)
  , uleb128, sleb128, LEB128, leb128, lebToU, lebToS
  , unumber
  , initialLength
  , string
  ) where

import           Data.Serialize.Get(Get)
import qualified Data.Serialize as S
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Word(Word8,Word16,Word32,Word64)
import           Data.Int(Int8)
import           Data.Bits(shiftL,clearBit,testBit,(.|.))

import DWARF.Utils(prettyHex)

-- | How to decode multi-byte numbers.
data Endian       = LittleEndian | BigEndian
                    deriving (Eq,Ord,Show)

-- | Abstraction of the DWARF container.
data Sections = Sections
  { secEndian   :: Endian
  , secBytes    :: Map String ByteString
  }

-- | Define a DWARF container, with the given byte-order, and sections.
sections :: Endian -> Map String ByteString -> Sections
sections endian mp = Sections { secEndian  = endian
                              , secBytes   = mp
                              }

-- | Accesss the bytes for a given section.
sectionBytes :: String -> Sections -> ByteString
sectionBytes x s = Map.findWithDefault BS.empty x (secBytes s)

-- | Access the byte-order for this container.
sectionEndian :: Sections -> Endian
sectionEndian = secEndian


-- | Decode a signed 8-bit number.
int8 :: Get Int8
int8 = S.getInt8

-- | Decode an unsigned 8-bit number.
word8 :: Get Word8
word8 = S.getWord8

-- | Decode an unsigned 16-bit number.
word16 :: Endian -> Get Word16
word16 i =
  case i of
    LittleEndian -> S.getWord16le
    BigEndian    -> S.getWord16be

-- | Decode an unsigned 32-bit number.
word32 :: Endian -> Get Word32
word32 i =
  case i of
    LittleEndian -> S.getWord32le
    BigEndian    -> S.getWord32be

-- | Decode an unsigned 64-bit number.
word64 :: Endian -> Get Word64
word64 i =
  case i of
    LittleEndian -> S.getWord64le
    BigEndian    -> S.getWord64be

-- | Decode a 0 terminated string.
-- The terminator is NOT part of the result,
-- but is extracted from the byte stream.
string :: Get ByteString
string = do bs <- S.getBytes =<< S.lookAhead (findZero 0)
            _  <- word8 -- skip the 0 terminator
            return bs
  where
  findZero !n = do b <- word8
                   if b == 0 then return n else findZero (n + 1)


data LEB128 = LEB !Integer !Int
            deriving (Show,Eq,Ord)

-- | Decode a DWARF multi-byte number.
-- The `Int` is the location of the sign bit, if the number is to be
-- interpreted as signed.
leb128 :: Get LEB128
leb128 = go 0 0
  where
  combine sh by res = shiftL (fromIntegral (clearBit by 7)) sh .|. res

  go !res !sh =
    do byte <- word8
       let newRes = combine sh byte res
       if testBit byte 7
         then go newRes (sh + 7)
         else do let signBit = sh + 6
                 return (LEB newRes signBit)


-- | Convert a decoded DWARF number to an unsigned number.
lebToU :: LEB128 -> Integer
lebToU (LEB x _) = x

-- | Convert a decoded DWARF number to a signed number.
lebToS :: LEB128 -> Integer
lebToS (LEB w sign)
  | testBit w sign  = fromIntegral w .|. negate (shiftL 1 sign)
  | otherwise       = w

-- | Decod a DWARF-encoded unsigned number.
uleb128 :: Get Integer
uleb128 = fmap lebToU leb128

-- | Decod a DWARF-encoded signed number.
sleb128 :: Get Integer
sleb128 = fmap lebToS leb128

-- | Decode the initial length of DWARF component.
-- This is common for many DWARF components---it returns the default
-- size for numbers, and the number of following bytes that are relevant.
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


-- | Default encoding for numbers.  Note that it is common to use `Dwarf32`
-- even on 64-bit machines, if the producer knows that 32-bits are plently
-- for the values it intends to store in the given object.
data DwarfFormat  = Dwarf32 | Dwarf64
                    deriving (Eq,Ord,Show)

-- | Decode an unsigned number with the default encoding of an object.
word :: Endian -> DwarfFormat-> Get Word64
word endian format =
  case format of
    Dwarf32 -> fromIntegral <$> word32 endian
    Dwarf64 -> word64 endian


-- | Decode a fixed-byte unsigned nubmer with the given endian-ness.
-- The second argument is the number of bytes in the number.
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



