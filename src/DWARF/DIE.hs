{-# LANGUAGE RecordWildCards #-}
-- | Processing of Debug Information Entries (DIE).
module DWARF.DIE where

import           Data.Serialize(Get)
import qualified Data.Serialize as S
import           Data.ByteString(ByteString)
import           Data.Word

import           DWARF.Basics
import           DWARF.Section.String
import           DWARF.Section.Abbrev

import           DWARF.DW.TAG
import           DWARF.DW.LANG
import           DWARF.DW.FORM
import           DWARF.DW.ATE
import           DWARF.DW.AT


data DIE = DIE
  { dieName       :: !DW_TAG
  , dieAttributes :: ![(DW_AT,AttributeValue)]
  , dieChildren   :: ![DIE]
  } deriving Show

lookupAT :: DW_AT -> DIE -> Maybe AttributeValue
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
  | Language  !DW_LANG
  | Encoding  !DW_ATE
    deriving Show


-- | Information needed for decoding of DIEs.
class DieMeta t where
  dieSections     :: t -> Sections
  dieFormat       :: t -> DwarfFormat
  dieAddressSize  :: t -> Word8
  dieAbbr         :: t -> Integer -> Maybe Abbreviation


{-
-- | Decode a single DIE.  Returns 'Nothing' if the DIE is blank (a terminator).
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
-}



-- | Decode a single DIE.  Returns 'Nothing' if the DIE is blank (a terminator).
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


-- | Decode a sequence of DIEs.
debugInfoEntries :: DieMeta t => t -> Get [DIE]
debugInfoEntries meta = go []
  where
  go as = do mb <- debugInfoEntry meta
             case mb of
               Nothing -> return (reverse as)
               Just a  -> go (a : as)

-- | Decode a list of attributes.
attributes ::
  DieMeta t => t -> [(DW_AT,DW_FORM)] -> Get [(DW_AT,AttributeValue)]
attributes meta xs = go [] xs
  where
  go vs []             = return (reverse vs)
  go vs ((a,f) : more) =
    do v <- attribute meta a f
       go ((a,v) : vs) more




-- | Decode the value of an attribue with the given name and description.
attribute :: DieMeta t => t -> DW_AT -> DW_FORM -> Get AttributeValue
attribute meta attr form0 = special <$> val form0

  where
  endian = sectionEndian (dieSections meta)

  special x =
    case (attr,x) of
      (DW_AT_language, Number n) -> Language (DW_LANG (fromIntegral n))
      (DW_AT_encoding, Number n) -> Encoding (DW_ATE  (fromIntegral n))
      _ -> x


  val form =
    case form of

      DW_FORM_addr -> Address <$> unumber endian (dieAddressSize meta)

      -- block
      DW_FORM_block   -> Block <$> (S.getBytes . fromIntegral =<< uleb128)
      DW_FORM_block1  -> Block <$> (S.getBytes . fromIntegral =<< word8)
      DW_FORM_block2  -> Block <$> (S.getBytes . fromIntegral =<< word16 endian)
      DW_FORM_block4  -> Block <$> (S.getBytes . fromIntegral =<< word32 endian)

      -- constant
      DW_FORM_data1 -> (Number . fromIntegral) <$> word8
      DW_FORM_data2 -> (Number . fromIntegral) <$> word16 endian
      DW_FORM_data4 -> (Number . fromIntegral) <$> word32 endian
      DW_FORM_data8 -> (Number . fromIntegral) <$> word64 endian
      DW_FORM_sdata -> (Number . fromIntegral) <$> sleb128
      DW_FORM_udata -> (Number . fromIntegral) <$> uleb128

      -- DWARF expression
      DW_FORM_exprloc -> ExprLoc <$> (S.getBytes . fromIntegral =<< uleb128)

      -- Boolean flag
      DW_FORM_flag         -> (Flag . (/= 0)) <$> word8
      DW_FORM_flag_present -> return (Flag True)

      -- lineptr, loclistptr, macptr, ranglistptr
      DW_FORM_sec_offset -> Offset <$> word endian (dieFormat meta)

      -- local references
      DW_FORM_ref1  -> (LocalRef . fromIntegral) <$> word8
      DW_FORM_ref2  -> (LocalRef . fromIntegral) <$> word16 endian
      DW_FORM_ref4  -> (LocalRef . fromIntegral) <$> word32 endian
      DW_FORM_ref8  -> (LocalRef . fromIntegral) <$> word64 endian
      DW_FORM_udata -> LocalRef                  <$> uleb128

      -- reference to debug_info
      DW_FORM_ref_addr -> RemoteRef <$> word endian (dieFormat meta)

      -- type entry, in its own type unit
      DW_FORM_ref_sig8 -> TypeRef <$> word64 endian

      DW_FORM_string -> String <$> string
      DW_FORM_strp ->
        do offset <- fromIntegral <$> word endian (dieFormat meta)
           case stringFrom (dieSections meta) offset of
             Left err -> fail err
             Right a  -> return (String a)

      DW_FORM_indirect -> val =<< ((DW_FORM . fromIntegral) <$> uleb128)

      _ -> fail ("Unknown attribute form: " ++ show form)





