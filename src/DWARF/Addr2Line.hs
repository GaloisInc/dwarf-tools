module DWARF.Addr2Line where

import Data.ByteString(ByteString)
import Data.Maybe(fromMaybe)
import Data.List(find)

import DWARF.Basics
import DWARF.DIE
import DWARF.Section.ARanges
import DWARF.Section.Info
import DWARF.Section.Line(getFile,File)
import DWARF.DW.TAG
import DWARF.DW.AT

data Info = Info
  { function :: Maybe ByteString
  , file     :: Maybe (File ByteString)
  , line     :: Maybe Integer
  } deriving Show

addr2line :: Sections -> Integer -> Info
addr2line secs n =
  fromMaybe Info { function = Nothing, file = Nothing, line = Nothing } $
  do off      <- findUnsegmentedAddr secs n
     (cu,die) <- fromEither (getCU secs off)
     die'     <- fromEither (loadChildren cu die)
     chi      <- fromEither (dieChildren die')
     decl0    <- find (containsAddr n) chi
     decl     <- case lookupAT DW_AT_specification decl0 of
                   Just (LocalRef r) ->
                     case getLocalDIE cu r of
                       Left err -> return decl0
                       Right d  -> return d
                   _ -> return decl0

     return Info { function = do String n <- lookupAT DW_AT_name decl
                                 return n
                 , file     = getFileName secs die decl
                 , line     = do Number n <- lookupAT DW_AT_decl_line decl
                                 return n
                 }

fromEither :: Either a b -> Maybe b
fromEither (Left _)  = Nothing
fromEither (Right a) = Just a


containsAddr :: Integer -> DIE -> Bool
containsAddr tgt d =
  case dieName d of
    DW_TAG_subprogram ->
      case (lookupAT DW_AT_low_pc d, lookupAT DW_AT_high_pc d) of
        (Just (Address a), Just (Number sz)) ->  tgt >= a && tgt < a + sz
        _ -> False
    _ -> False


getFileName :: Sections -> DIE -> DIE -> Maybe (File ByteString)
getFileName secs cu decl =
  do Offset w <- lookupAT DW_AT_stmt_list cu
     Number f <- lookupAT DW_AT_decl_file decl
     case getFile secs w f of
       Left _ -> Nothing
       Right f -> Just f

