-- import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(isPrefixOf)
import Data.Elf
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
import DW.Sections
import DW.Section.Abbrev
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
         secs = Map.fromList
                  [ (name, elfSectionData s)
                    | s <- elfSections elf
                    , let name = elfSectionName s
                    , ".debug_" `isPrefixOf` name ]

     putStrLn "Debug sections:"
     mapM_ putStrLn (Map.keys secs)

     ss <- case sections end secs of
             Left err -> fail err
             Right a  -> return a

     putStrLn "\n-- ARanges -----------\n"

     ar <- case aranges ss of
             Left err -> fail err
             Right a  -> pPrint a >> return a

     putStrLn "\n-- Info -----------\n"

     info_bytes <- case Map.lookup ".debug_info" secs of
                     Nothing -> fail "Can't find debug info"
                     Just sec_bytes -> return sec_bytes


     case args of
       a : _ | Just n <- readMaybe a ->
         case findUnsegmentedAddr ar n of
           Nothing -> putStrLn "(not found)"
           Just off ->
             case dieFrom ss off of
               Left err -> fail err
               Right die ->
                  do putStrLn "Found in DIE:"
                     pPrint die
                     case lookupAT Stmt_list die of
                       Just (Offset w) ->
                          case L.findLineInfo ss w (\r -> L.address r >= n) of
                            Just y -> pPrint y
                            Nothing -> fail "Can't find"

                       _ -> fail "Can't fine stmt list"

       a : _ -> putStrLn ("(Failed to parse: " ++ a ++ ")")
       [] -> putStrLn "(nothing to do)"


