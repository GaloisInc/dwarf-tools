{-# LANGUAGE RecordWildCards #-}
module DWARF.Section.Abbrev where

import           Data.Serialize(Get,runGet)
import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import           Data.Word(Word64)

import DWARF.Basics
import DWARF.DW.TAG
import DWARF.DW.AT
import DWARF.DW.FORM

abbrev :: Sections -> Word64 -> Either String (Map Integer Abbreviation)
abbrev secs off = runGet abbreviations bytes
  where bytes = BS.drop (fromIntegral off) (sectionBytes ".debug_abbrev" secs)


data Abbreviation = Abbreviation
  { abbrTag         :: !DW_TAG
  , abbrHasChildren :: !Bool
  , abbrAttrs       :: ![(DW_AT,DW_FORM)]
  } deriving Show


abbreviations :: Get (Map Integer Abbreviation)
abbreviations = Map.fromList <$> go []
  where
  go as = do uid <- uleb128
             if uid == 0
                then return as
                else do a  <- abbreviation
                        go ((uid,a) : as)

abbreviation :: Get Abbreviation
abbreviation =
  do abbrTag         <- (DW_TAG . fromIntegral) <$> uleb128
     abbrHasChildren <- hasChildren
     abbrAttrs       <- attributeSpecs
     return Abbreviation { .. }

hasChildren :: Get Bool
hasChildren =
  do b <- word8
     case b of
       0x00 -> return False
       0x01 -> return True
       _    -> fail ("hasChildren: " ++ show b)

attributeSpecs :: Get [(DW_AT,DW_FORM)]
attributeSpecs =
  do mb <- attributeSpec
     case mb of
       (DW_AT_NULL,DW_FORM_NULL) -> return []
       _                         -> do as <- attributeSpecs
                                       return (mb : as)

attributeSpec :: Get (DW_AT,DW_FORM)
attributeSpec =
  do t <- (DW_AT   . fromInteger) <$> uleb128
     f <- (DW_FORM . fromInteger) <$> uleb128
     return (t,f)








