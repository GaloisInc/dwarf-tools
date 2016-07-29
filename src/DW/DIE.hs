{-# LANGUAGE RecordWildCards #-}
module DW.DIE where

import           Data.Serialize(Get)
import qualified Data.Serialize as S
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Word

import           DW.Basics
import           DW.Sections
import           DW.Section.String
import           DW.Section.Abbrev
import           DW.TAG
import           DW.LANG(LANG(..))
import           DW.FORM(FORM(..))
import qualified DW.FORM
import           DW.ATE(ATE(..))
import           DW.AT(AT(..))
import qualified DW.AT


data DIE = DIE
  { dieName       :: !TAG
  , dieAttributes :: [(AT,AttributeValue)]
  , dieChildren   :: [DIE]
  } deriving Show

lookupAT :: AT -> DIE -> Maybe AttributeValue
lookupAT a d = lookup a (dieAttributes d)


data AttributeValue =
    Address   !Integer
  | Block     !ByteString
  | Number    !Integer
  | ExprLoc   !ByteString
  | Flag      !Bool
  | Offset    !Word64     -- Reference in some section
  | LocalRef  !Integer
  | RemoteRef !Word64
  | TypeRef   !Word64
  | String    !ByteString
  | Language  !LANG
  | Encoding  !ATE
    deriving Show


class DieMeta t where
  dieSections     :: t -> Sections
  dieFormat       :: t -> DwarfFormat
  dieAddressSize  :: t -> Word8
  dieAbbr         :: t -> Integer -> Maybe Abbreviation

debugInfoEntryFrom ::
  DieMeta t => t -> Word64 -> ByteString -> Either String DIE
debugInfoEntryFrom meta offset bytes =
  case S.runGet (debugInfoEntry meta) (BS.drop (fromIntegral offset) bytes) of
    Left err       -> Left err
    Right Nothing  -> Left "(Missing debug info entry)"
    Right (Just a) -> Right a


debugInfoEntry :: DieMeta t => t -> Get (Maybe DIE)
debugInfoEntry meta =
  do abbr <- uleb128
     if abbr == 0
       then return Nothing
       else case dieAbbr meta abbr of
              Just descr ->
                do dieAttributes <- attributes meta (abbrAttrs descr)
                   dieChildren   <- if abbrHasChildren descr
                                       then debugInfoEntries meta
                                       else return []
                   return (Just DIE { dieName = abbrTag descr, .. })
              Nothing    -> fail ("Unknown abbreviation: " ++ show abbr)


debugInfoEntries :: DieMeta t => t -> Get [DIE]
debugInfoEntries meta = go []
  where
  go as = do mb <- debugInfoEntry meta
             case mb of
               Nothing -> return (reverse as)
               Just a  -> go (a : as)

attributes :: DieMeta t => t -> [(AT,FORM)] -> Get [(AT,AttributeValue)]
attributes meta xs = go [] xs
  where
  go vs []             = return (reverse vs)
  go vs ((a,f) : more) =
    do v <- attribute meta a f
       go ((a,v) : vs) more





attribute :: DieMeta t => t -> AT -> FORM -> Get AttributeValue
attribute meta attr form0 = special <$> val form0

  where
  endian = secEndian (dieSections meta)

  special x =
    case (attr,x) of
      (DW.AT.Language, Number n) -> Language (LANG (fromIntegral n))
      (DW.AT.Encoding, Number n) -> Encoding (ATE (fromIntegral n))
      _ -> x


  val form =
    case form of

      DW.FORM.Addr -> Address <$> unumber endian (dieAddressSize meta)

      -- block
      DW.FORM.Block   -> Block <$> (S.getBytes . fromIntegral =<< uleb128)
      DW.FORM.Block1  -> Block <$> (S.getBytes . fromIntegral =<< word8)
      DW.FORM.Block2  -> Block <$> (S.getBytes . fromIntegral =<< word16 endian)
      DW.FORM.Block4  -> Block <$> (S.getBytes . fromIntegral =<< word32 endian)

      -- constant
      DW.FORM.Data1 -> (Number . fromIntegral) <$> word8
      DW.FORM.Data2 -> (Number . fromIntegral) <$> word16 endian
      DW.FORM.Data4 -> (Number . fromIntegral) <$> word32 endian
      DW.FORM.Data8 -> (Number . fromIntegral) <$> word64 endian
      DW.FORM.Sdata -> (Number . fromIntegral) <$> sleb128
      DW.FORM.Udata -> (Number . fromIntegral) <$> uleb128

      -- DWARF expression
      DW.FORM.Exprloc -> ExprLoc <$> (S.getBytes . fromIntegral =<< uleb128)

      -- Boolean flag
      DW.FORM.Flag         -> (Flag . (/= 0)) <$> word8
      DW.FORM.Flag_present -> return (Flag True)

      -- lineptr, loclistptr, macptr, ranglistptr
      DW.FORM.Sec_offset -> Offset <$> word endian (dieFormat meta)

      -- local references
      DW.FORM.Ref1  -> (LocalRef . fromIntegral) <$> word8
      DW.FORM.Ref2  -> (LocalRef . fromIntegral) <$> word16 endian
      DW.FORM.Ref4  -> (LocalRef . fromIntegral) <$> word32 endian
      DW.FORM.Ref8  -> (LocalRef . fromIntegral) <$> word64 endian
      DW.FORM.Udata -> LocalRef                  <$> uleb128

      -- reference to debug_info
      DW.FORM.Ref_addr -> RemoteRef <$> word endian (dieFormat meta)

      -- type entry, in its own type unit
      DW.FORM.Ref_sig8 -> TypeRef <$> word64 endian

      DW.FORM.String -> String <$> string
      DW.FORM.Strp ->
        do offset <- fromIntegral <$> word endian (dieFormat meta)
           case stringFrom (dieSections meta) offset of
             Left err -> fail err
             Right a  -> return (String a)

      DW.FORM.Indirect -> val =<< ((FORM . fromIntegral) <$> uleb128)

      _ -> fail ("Unknown attribute form: " ++ show form)





