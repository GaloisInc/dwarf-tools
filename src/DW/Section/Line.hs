{-# LANGUAGE RecordWildCards #-}
module DW.Section.Line where

import Data.Word
import Data.Int
import Data.Serialize
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Control.Monad(replicateM_)

import DW.Basics
import DW.Sections
import DW.LNS
import DW.Section.String

findLineInfo :: Sections -> Word64 -> (LineInfo -> Bool) -> Maybe LineInfo
findLineInfo secs offset p =
  case runGet (search (secEndian secs) p) bytes of
    Left e  -> Nothing
    Right r -> r
  where bytes = BS.drop (fromIntegral offset) (sectionBytes ".debug_line" secs)



data LineInfo = LineInfo Header State

toLineInfo :: Header -> State -> LineInfo
toLineInfo = LineInfo

address :: LineInfo -> Integer
address (LineInfo _ s) = sAddress s

opIndex :: LineInfo -> Integer
opIndex (LineInfo _ s) = sOpIndex s

file :: LineInfo -> File ByteString
file (LineInfo h s)
  | fid < 1 = dummyFile
  | (_,b:_) <- splitAt (fromInteger fid - 1) allFiles =
      let dirIx = fromInteger (directory b)
      in b { directory = case splitAt dirIx (include_directories h) of
                           (_,d:_) -> d
                           _       -> BS.empty }
  | otherwise = dummyFile

  where
  fid = sFile s
  allFiles = file_names h ++ reverse (sExtraFiles s)
  dummyFile = File { fileName     = BS.empty
                   , directory    = BS.empty
                   , lastModified = 0
                   , fileSize     = 0
                   }

line :: LineInfo -> Integer
line (LineInfo _ s) = sLine s

column :: LineInfo -> Integer
column (LineInfo _ s) = sColumn s

recommendedBreak :: LineInfo -> Bool
recommendedBreak (LineInfo _ s) = sIsStmt s

startBasicBlock :: LineInfo -> Bool
startBasicBlock (LineInfo _ s) = sBasicBlock s

sequenceEnded :: LineInfo -> Bool
sequenceEnded (LineInfo _ s) = sEndSequence s

prologueEnd :: LineInfo -> Bool
prologueEnd (LineInfo _ s) = sPrologueEnd s

epilogueBegin :: LineInfo -> Bool
epilogueBegin (LineInfo _ s) = sEpilogueBegin s

isa :: LineInfo -> Integer
isa (LineInfo _ s) = sISA s


instance Show LineInfo where
  show l = unlines
    [ "{ address       = " ++ prettyHex (address l)
    , ", op_ix         = " ++ show (opIndex l)
    , ", line          = " ++ show (line l)
    , ", column        = " ++ show (column l)
    , ", break         = " ++ show (recommendedBreak l)
    , ", startBlock    = " ++ show (startBasicBlock l)
    , ", sequenceEnd   = " ++ show (sequenceEnded l)
    , ", prologueEnd   = " ++ show (prologueEnd l)
    , ", epilogueBegin = " ++ show (epilogueBegin l)
    , ", isa           = " ++ show (isa l)
    , "}"
    ]




data Header = Header
  { format                             :: !DwarfFormat
  , size                               :: !Word64
  , version                            :: !Word16
  , header_length                      :: !Word64
  , minimum_instruction_length         :: !Word8
  , maximum_operations_per_instruction :: !Word8
  , default_is_stmt                    :: !Bool
  , line_base                          :: !Int8
  , line_range                         :: !Word8
  , opcode_base                        :: !Word8
  , standard_opcode_lengths            :: !ByteString -- Array of Word8
  , include_directories                :: ![ByteString]
  , file_names                         :: ![File Integer]
  }


limitBytes :: (Integral n) => n -> Get a -> Get a
limitBytes x g =
  isolate (fromIntegral x) $
    do a <- g
       skip =<< remaining
       return a


withHeader :: Endian -> (Header -> Get a) -> Get a
withHeader endian k =
  do (format,size) <- initialLength endian
     limitBytes size $
       do version <- word16 endian
          header_length <- word endian format
          hdr <- limitBytes header_length $
            do minimum_instruction_length         <- word8
               maximum_operations_per_instruction <-
                 if version > 3 then word8 else return 1
               default_is_stmt <- (== 0) <$> word8
               line_base       <- int8
               line_range      <- word8
               opcode_base     <- word8
               standard_opcode_lengths <-
                 getBytes (fromIntegral opcode_base - 1)
               include_directories <- getDirs []
               file_names          <- getFiles []
               return Header { .. }
          k hdr

  where
  getDirs ds = do done <- lookAhead $ fmap (== 0) word8
                  if done then word8 >> return (reverse ds)
                          else do d <- string
                                  getDirs (d : ds)

  getFiles fs = do done <- lookAhead ((== 0) <$> word8)
                   if done then word8 >> return (reverse fs)
                           else do f <- fileEntry
                                   getFiles (f : fs)

data State = State
  { sAddress        :: !Integer
  , sOpIndex        :: !Integer
  , sFile           :: !Integer
  , sLine           :: !Integer
  , sColumn         :: !Integer
  , sIsStmt         :: !Bool
  , sBasicBlock     :: !Bool
  , sEndSequence    :: !Bool
  , sPrologueEnd    :: !Bool
  , sEpilogueBegin  :: !Bool
  , sISA            :: !Integer
  , sDiscriminator  :: !Integer
  , sExtraFiles     :: ![File Integer] -- REVERSED
  } deriving Show


initState :: Header -> State
initState Header { .. } = State
  { sAddress        = 0
  , sOpIndex        = 0
  , sFile           = 1
  , sLine           = 1
  , sColumn         = 0
  , sIsStmt         = default_is_stmt
  , sBasicBlock     = False
  , sEndSequence    = False
  , sPrologueEnd    = False
  , sEpilogueBegin  = False
  , sISA            = 0
  , sDiscriminator  = 0
  , sExtraFiles     = []
  }



opCode :: Endian -> Header -> State -> Get (Maybe LineInfo, State)
opCode endian h s =
  do u <- word8
     case u of
       0 -> do bNum <- uleb128
               isolate (fromIntegral bNum)
                 $ do op <- Extended <$> word8
                      extendedOpCode endian h op s
       _ | u < opcode_base h -> standardOpCode endian h (Standard u) s
         | otherwise         -> let (r,s1) = specialOpCode h u s
                                in return (Just r, s1)



advanceAddress :: Header -> Integer -> State -> State
advanceAddress h amt s =
  s { sAddress = sAddress s + fromIntegral (minimum_instruction_length h) * is
    , sOpIndex = ofs
    }
  where
  opAdvance = div amt (fromIntegral (line_range h))
  (is,ofs)  = divMod (sOpIndex s + opAdvance)
                     (fromIntegral (maximum_operations_per_instruction h))


advanceLine :: Header -> Integer -> State -> State
advanceLine h amt s = s { sLine = sLine s + inc }
  where
  inc = fromIntegral (line_base h) + mod amt (fromIntegral (line_range h))


specialOpCode :: Header -> Word8 -> State -> (LineInfo, State)
specialOpCode h op s = (toLineInfo h s1, s1 { sBasicBlock    = False
                                     , sPrologueEnd   = False
                                     , sEpilogueBegin = False
                                     , sDiscriminator = 0
                                     })
  where
  adj = fromIntegral (op - opcode_base h)
  s1  = advanceLine h adj (advanceAddress h adj s)




standardOpCode :: Endian -> Header -> Standard -> State -> Get (Maybe LineInfo,State)
standardOpCode endian h op s =
  case op of
    Copy -> return ( Just (toLineInfo h s)
                   , s { sDiscriminator = 0
                       , sBasicBlock    = False
                       , sPrologueEnd   = False
                       , sEpilogueBegin = False
                       } )
    Advance_pc ->
      do amt <- uleb128
         -- XXX: This looks like a bug in the standard v3 vs v4?
         -- The questoin is: should devinde by line range or not?
         return (Nothing,
            if version h > 3
               then advanceAddress h amt s
               else s { sAddress = sAddress s +
                          amt * fromIntegral (minimum_instruction_length h) })

    Advance_line ->
      do amt <- sleb128
         return (Nothing, s { sLine = amt + sLine s })


    Set_file ->
      do f <- uleb128
         return (Nothing, s { sFile = f })

    Set_column ->
      do c <- uleb128
         return (Nothing, s { sColumn = c })

    Negate_stmt -> return (Nothing, s { sIsStmt = not (sIsStmt s) })

    Set_basic_block ->
      return (Nothing, s { sBasicBlock = True })

    Const_add_pc -> return (Nothing, advanceAddress h adj s)
       where adj = fromIntegral (255 - opcode_base h)

    Fixed_advance_pc ->
      do amt <- word16 endian
         return (Nothing, s { sAddress = fromIntegral amt + sAddress s
                            , sOpIndex = 0
                             })

    Set_prologue_end ->
      return (Nothing, s { sPrologueEnd = True })

    Set_epilogue_begin ->
      return (Nothing, s { sEpilogueBegin = True })

    Set_isa ->
      do isa <- uleb128
         return (Nothing, s { sISA = isa })

    -- Unknown op-code, we just treat is a no-op; this may or may not be ok
    Standard x
      | ix < BS.length (standard_opcode_lengths h) ->
        do let arity = fromIntegral (BS.index (standard_opcode_lengths h) ix)
           replicateM_ arity uleb128
           return (Nothing, s)
      | otherwise ->
          fail ("Unknown standard op-code with no arith: " ++ prettyHex x)
      where ix = fromIntegral (x - 1)


extendedOpCode ::
  Endian -> Header -> Extended -> State -> Get (Maybe LineInfo, State)
extendedOpCode endian h op@(Extended zz) s =
  case op of

    End_sequence ->
      let s1 = s { sEndSequence = True }
      in return (Just (toLineInfo h s1), initState h)

    Set_address ->
      do n <- remaining
         a <- unumber endian (fromIntegral n)
         return (Nothing, s { sAddress = a, sOpIndex = 0 })

    Define_file ->
      do f <- fileEntry
         return (Nothing, s { sExtraFiles = f : sExtraFiles s })

    Set_discriminator ->
      do n <- uleb128
         return (Nothing, s { sDiscriminator = n })

    -- We treat others as no-op
    Extended _ ->
      do skip =<< remaining
         return (Nothing, s)


search :: Endian -> (LineInfo -> Bool) -> Get (Maybe LineInfo)
search endian p =
  withHeader endian $ \h -> go h (initState h)
  where
  go h s = do done <- isEmpty
              if done
                then return Nothing
                else do (mb,s1) <- opCode endian h s
                        case mb of
                          Just r | p r -> return (Just r)
                          _ -> go h s1



data File dir = File { fileName     :: !ByteString
                     , directory    :: !dir
                     , lastModified :: !Integer
                     , fileSize     :: !Integer
                     } deriving Show

fileEntry :: Get (File Integer)
fileEntry =
  do fileName     <- string
     directory    <- uleb128
     lastModified <- uleb128
     fileSize     <- uleb128
     return File { .. }

