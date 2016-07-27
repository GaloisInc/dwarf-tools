{-# LANGUAGE RecordWildCards #-}
module DW.Section.Abbrev where

import           Data.Serialize(Get,runGet)
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.ByteString(ByteString)

import           DW.Basics
import           DW.TAG(TAG(..))
import           DW.AT(AT(..))
import qualified DW.AT
import           DW.FORM(FORM(..))
import qualified DW.FORM

abbrev :: ByteString -> Either String (Map Integer Abbreviation)
abbrev bytes = runGet abbreviations bytes


data Abbreviation = Abbreviation
  { abbrTag         :: !TAG
  , abbrHasChildren :: !Bool
  , abbrAttrs       :: [(AT,FORM)]
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
  do abbrTag         <- (TAG . fromIntegral) <$> uleb128
     abbrHasChildren <- hasChildren
     abbrAttrs       <- attributeSpecs
     return Abbreviation { .. }

hasChildren :: Get Bool
hasChildren =
  do b <- word8
     case b of
       0x00 -> return True
       0x01 -> return False
       _    -> fail ("hasChildren: " ++ show b)

attributeSpecs :: Get [(AT,FORM)]
attributeSpecs =
  do mb <- attributeSpec
     case mb of
       (DW.AT.NULL,DW.FORM.NULL) -> return []
       _                         -> do as <- attributeSpecs
                                       return (mb : as)

attributeSpec :: Get (AT,FORM)
attributeSpec =
  do t <- (AT   . fromInteger) <$> uleb128
     f <- (FORM . fromInteger) <$> uleb128
     return (t,f)








