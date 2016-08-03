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
import           DWARF.DW.INL


data DIE = DIE
  { dieName       :: !DW_TAG
  , dieAttributes :: ![(DW_AT,AttributeValue)]
  , dieChildren   :: Children  -- It is important that this is lazy!
  } deriving Show

data Children = NoChildren
              | ChildError String
              | Child DIE Children

instance Show Children where
  showsPrec p cs = showChar '[' . show1 cs . showChar ']'
    where
    showERR txt = showString "ERROR " . shows txt

    show1 NoChildren       = id
    show1 (ChildError err) = showERR err
    show1 (Child d more)   = shows d . show2 more

    show2 NoChildren       = id
    show2 x                = showString ", " . show1 x


-- | Check if the children contain an error
badChildren :: Children -> Maybe String
badChildren chi =
  case chi of
    NoChildren      -> Nothing
    ChildError err  -> Just err
    Child _ cs      -> badChildren cs

findChild :: (DIE -> Bool) -> Children -> Either String (Maybe DIE)
findChild p chi =
  case chi of
    NoChildren     -> Right Nothing
    ChildError err -> Left err
    Child d more   -> if p d then Right (Just d) else findChild p more


newtype BS = BS ByteString

instance Show BS where
  show _ = "EncodedChildren"



lookupAT :: DW_AT -> DIE -> Maybe AttributeValue
lookupAT a d = lookup a (dieAttributes d)


data AttributeValue =
    Address   !Integer
  | Block     !ByteString
  | Number    !Integer
  | ExprLoc   !ByteString
  | Flag      !Bool
  | Offset    !Word64     -- Reference in some section

  | LocalRef  !Word64     -- ^ DIE; offset from start of CU
  | RemoteRef !Word64     -- ^ DIE; offset from start of .debug_info
  | TypeRef   !Word64     -- ^ DIE; identifier in .debug_types

  | String    !ByteString
  | Language  !DW_LANG
  | Encoding  !DW_ATE
  | Inline    !DW_INL
    deriving Show


-- | Information needed for decoding of DIEs.
class HasDIE t where
  dieSections     :: t -> Sections
  dieFormat       :: t -> DwarfFormat
  dieAddressSize  :: t -> Word8
  dieAbbr         :: t -> Integer -> Maybe Abbreviation
  dieLocallBytes  :: t -> Word64 -> ByteString
  -- ^ Get the bytes for local reference.
  -- We DON'T assume anythong about the size of the bytestring
  -- (i.e., it will likely contain *more* than the local entry)




-- | Decode the outline of a DIE.
bsDIE :: HasDIE cu =>
         cu ->
         ByteString ->
         Either String
                (Maybe (DW_TAG,Bool,[(DW_AT,AttributeValue)]), ByteString)
bsDIE cu bytes = S.runGetState die bytes 0
  where
  die = do abbr <- uleb128
           if abbr == 0
              then return Nothing
              else case dieAbbr cu abbr of
                     Nothing -> fail ("Unknown abbrevation: " ++ show abbr)
                     Just descr ->
                       do dieAttributes <- attributes cu (abbrAttrs descr)
                          return (Just ( abbrTag descr
                                       , abbrHasChildren descr
                                       , dieAttributes
                                       ))

-- | Decode children lazyily.
lazyChildren :: HasDIE cu => cu -> ByteString -> Children
lazyChildren cu = fst . go
  where
  go bytes =
    case bsDIE cu bytes of
      Left err      -> (ChildError err, error "[BUG] lazyByteChildren")
      Right (mb,rest) ->
        case mb of
          Nothing -> (NoChildren, rest)
          Just (tag,childs,attrs) ->
            let (ourChildren,afterOurChildren)
                  | childs    = go rest
                  | otherwise = (NoChildren, rest)

                d = DIE { dieName       = tag
                        , dieAttributes = attrs
                        , dieChildren   = ourChildren
                        }
                (otherChildren,next) =
                   case lookup DW_AT_sibling attrs of
                     Just (LocalRef off) -> go (dieLocallBytes cu off)
                     Nothing ->
                       case badChildren ourChildren of
                         Just err -> (ChildError err,afterOurChildren)
                         Nothing  -> go afterOurChildren
            in (Child d otherChildren,next)


decodeDIE :: HasDIE cu => cu -> ByteString -> Either String DIE
decodeDIE cu bs =
  do (mb,rest) <- bsDIE cu bs
     case mb of
       Nothing -> Left "(no die)"

       -- We check for children first, so if there are none we don't hold to BS
       Just (tag,hasChildren,attr)
         | hasChildren ->
            Right DIE { dieName       = tag
                      , dieAttributes = attr
                      , dieChildren   = lazyChildren cu rest
                      }
         | otherwise -> Right DIE { dieName       = tag
                                  , dieAttributes = attr
                                  , dieChildren   = NoChildren
                                  }


getLocalDIE :: HasDIE t => t -> Word64 -> Either String DIE
getLocalDIE cu off = decodeDIE cu (dieLocallBytes cu off)




-- | Decode a list of attributes.
attributes :: HasDIE t => t -> [(DW_AT,DW_FORM)] -> Get [(DW_AT,AttributeValue)]
attributes meta xs = go [] xs
  where
  go vs []             = return (reverse vs)
  go vs ((a,f) : more) =
    do v <- attribute meta a f
       go ((a,v) : vs) more


-- | Decode the value of an attribue with the given name and description.
attribute :: HasDIE t => t -> DW_AT -> DW_FORM -> Get AttributeValue
attribute meta attr form0 = special <$> val form0

  where
  endian = sectionEndian (dieSections meta)

  special x =
    case (attr,x) of
      (DW_AT_language, Number n) -> Language (DW_LANG (fromIntegral n))
      (DW_AT_encoding, Number n) -> Encoding (DW_ATE  (fromIntegral n))
      (DW_AT_inline,   Number n) -> Inline (DW_INL (fromIntegral n))
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
      DW_FORM_ref1  -> (LocalRef . fromIntegral)  <$> word8
      DW_FORM_ref2  -> (LocalRef . fromIntegral)  <$> word16 endian
      DW_FORM_ref4  -> (LocalRef . fromIntegral)  <$> word32 endian
      DW_FORM_ref8  -> (LocalRef . fromIntegral)  <$> word64 endian
      DW_FORM_udata -> (LocalRef  . fromIntegral) <$> uleb128

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





