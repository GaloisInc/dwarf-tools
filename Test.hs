-- import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(isPrefixOf)
import Data.Elf
import Data.List(find)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import System.Environment
import qualified Data.Serialize as S
import Text.Show.Pretty-- (pPrint,ppShow)
import Hexdump
import Text.Read(readMaybe)

-- import Dwarf

import DW.Basics hiding (prettyHex)
import DW.DIE
import DW.AT
import DW.TAG
import DW.Sections
import DW.Section.Info
import DW.Section.ARanges
import qualified DW.Section.Line as L


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
             case dieFrom secs off of
               Left err -> fail err
               Right cu ->
                  do putStrLn "Found CU"
                     -- pPrint cu
                     case find (containsAddr n) (dieChildren cu) of
                       Nothing -> fail "Can't find decl"
                       Just decl ->
                         do pPrint (lookupAT Name decl)
                            pPrint (getFileName secs cu decl)
                            pPrint (lookupAT Decl_line decl)

       a : _ -> putStrLn ("(Failed to parse: " ++ a ++ ")")
       [] -> putStrLn "(nothing to do)"


containsAddr :: Integer -> DIE -> Bool
containsAddr tgt d =
  case dieName d of
    Subprogram ->
      case (lookupAT Low_pc d, lookupAT High_pc d) of
        (Just (Address a), Just (Number sz)) ->  tgt >= a && tgt < a + sz
        _ -> False
    _ -> False


getFileName :: Sections -> DIE -> DIE -> Maybe (L.File ByteString)
getFileName secs cu decl =
  do Offset w <- lookupAT Stmt_list cu
     Number f <- lookupAT Decl_file decl
     case L.getFile secs w f of
       Left _ -> Nothing
       Right f -> Just f

