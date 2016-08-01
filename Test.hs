-- import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(isPrefixOf)
import Data.Elf
import Data.List(find)
import Data.Maybe(maybeToList)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import System.Environment
import Text.Show.Pretty-- (pPrint,ppShow)
import Text.Read(readMaybe)


import DWARF.Basics
import DWARF.DIE
import DWARF.Section.Line
import DWARF.Section.Info
import DWARF.Section.ARanges
import DWARF.DW.AT
import DWARF.DW.TAG


main :: IO ()
main =
  do args <- getArgs
     let a = "/home/diatchki/src/galua/galua-c/inplace/bin/galua-dbg"
     bs <- BS.readFile a
     let elf  = parseElf bs
         end  = case elfData elf of
                  ELFDATA2LSB -> LittleEndian
                  ELFDATA2MSB -> BigEndian
         secs = sections end
              $ Map.fromList
                  [ (name, elfSectionData s)
                    | s <- elfSections elf
                    , let name = elfSectionName s
                    , ".debug_" `isPrefixOf` name ]


     case args of
       a : _ | Just n <- readMaybe a ->
         case findUnsegmentedAddr secs n of
           Nothing -> putStrLn "(not found)"
           Just off ->
             case getCU secs off of
               Left err -> fail err
               Right (cu,die) ->
                  do putStrLn "Found CU"
                     case loadChildren cu die of
                       Left err -> fail err
                       Right a ->
                         do let Right cs = dieChildren a
                            case find (containsAddr n) cs of
                              Nothing -> fail "Can't find decl"
                              Just decl ->
                                do pPrint (lookupAT DW_AT_name decl)
                                   pPrint (getFileName secs die decl)
                                   pPrint (lookupAT DW_AT_decl_line decl)

       a : _ -> putStrLn ("(Failed to parse: " ++ a ++ ")")
       [] -> putStrLn "(nothing to do)"


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

