module DWARF.Addr2Line where 
import Data.ByteString(ByteString)
import Data.Maybe(fromMaybe)
import Data.List(find)
import Data.Word(Word64)
import Control.Monad(join)

import DWARF.Basics
import DWARF.DIE
import DWARF.Section.ARanges
import DWARF.Section.Info
import DWARF.Section.Line(File)
import qualified DWARF.Section.Line as L
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
     decl0    <- join $ fromEither
                      $ findChild (containsAddr n) (dieChildren die)
     decl     <- fromEither (resovleIndirections cu decl0)

     let mb = getFileNameAndLine secs die decl n

     return Info { function = do String n <- lookupAT DW_AT_name decl
                                 return n
                 , file     = fst <$> mb
                 , line     = snd <$> mb
                 }

fromEither :: Either a b -> Maybe b
fromEither (Left _)  = Nothing
fromEither (Right a) = Just a

resovleIndirections :: CU -> DIE -> Either String DIE
resovleIndirections cu d
  | Just (LocalRef r) <- lookupAT DW_AT_specification d =
      resovleIndirections cu =<< getLocalDIE cu r
  | Just (LocalRef r) <- lookupAT DW_AT_abstract_origin d =
      resovleIndirections cu =<< getLocalDIE cu r
  | otherwise = Right d


containsAddr :: Integer -> DIE -> Bool
containsAddr tgt d =
  case dieName d of
    DW_TAG_subprogram ->
      case (lookupAT DW_AT_low_pc d, lookupAT DW_AT_high_pc d) of
        (Just (Address a), Just (Number sz)) ->  tgt >= a && tgt < a + sz
        (Just (Address a), Just (Address b)) ->  tgt >= a && tgt < b
        _ ->  False
    _ -> False


getFileNameAndLine :: Sections -> DIE -> DIE -> Integer ->
    Maybe (File ByteString, Integer)
getFileNameAndLine secs cu decl addr =
  do Offset w <- lookupAT DW_AT_stmt_list cu
     info <- L.findLineInfo secs w (\info -> L.address info >= addr)
     return (L.file info, L.line info)






